#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/grammar/algebra
        :gerbil-parser/src/grammar/lexical-algebra
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/compiler/lr
        :gerbil-parser/src/modules/parser/objects
        :gerbil-parser/src/runtime/recognition
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/languages/arithmetic/v1/parser)

(defgrammar-role conflicting-lexical-role
  (syntax-kinds
   (Punctuation token (text)))
  (terminals
   (punctuation Punctuation))
  (lexical-rules
   (punctuation (literals "+" "-" "*" "/" "(" ")")))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar conflicting-arithmetic-grammar
  (supers arithmetic-grammar)
  (roles conflicting-lexical-role))

(defgrammar-role unresolved-reference-role
  (syntax-kinds
   (SourceFile node ())
   (Unknown token (text)))
  (terminals
   (unknown Unknown))
  (lexical-rules
   (unknown (fallback)))
  (rules
   (source-file (reference missing-rule)))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar unresolved-reference-grammar
  (supers)
  (roles unresolved-reference-role))

(defgrammar-role invalid-terminal-kind-role
  (syntax-kinds
   (SourceFile node (value)))
  (terminals
   (value SourceFile))
  (lexical-rules
   (value (identifier)))
  (rules
   (source-file
    (alias SourceFile (field value (token value)))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar invalid-terminal-kind-grammar
  (supers)
  (roles invalid-terminal-kind-role))

(defgrammar-role missing-lexical-rule-role
  (syntax-kinds
   (SourceFile node (value))
   (Identifier token (text)))
  (terminals
   (identifier Identifier))
  (lexical-rules)
  (rules
   (source-file
    (alias SourceFile (field value (token identifier)))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar missing-lexical-rule-grammar
  (supers)
  (roles missing-lexical-rule-role))

(defgrammar-role unsupported-precedence-role
  (syntax-kinds
   (SourceFile node (value))
   (Identifier token (text)))
  (terminals
   (identifier Identifier))
  (lexical-rules
   (identifier (identifier)))
  (rules
   (source-file
    (alias SourceFile
      (field value
        (prec left 10 (token identifier))))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar unsupported-precedence-grammar
  (supers)
  (roles unsupported-precedence-role))

(def (condition-message thunk)
  (with-catch (lambda (condition) (error-message condition)) thunk))

(def (compile-composed grammar)
  (compile-parser (compile-grammar grammar)))

(def dynamic-precedence-rules
  '((source-file
     (precedence dynamic 1
      (alias SourceFile (field value (token identifier)))))))

(def ambiguous-left-recursive-rules
  '((source-file
     (choice
      (sequence (reference source-file) (reference source-file))
      (alias SourceFile (field value (token identifier)))))))

(def selective-session-rules
  '((source-file
     (alias SourceFile
      (sequence (reference session-activity)
                (optional (reference session-close)))))
    (session-activity (repeat1 (reference session-set)))
    (session-set (sequence (literal "SESSION") (literal "SET")))
    (session-close (sequence (literal "SESSION") (literal "CLOSE")))))

(def nonassociative-rules
  '((source-file
     (alias SourceFile (field expression (reference expression))))
    (expression
     (choice
      (precedence none 10
       (alias ComparisonExpression
        (sequence
         (field left (reference expression))
         (field operator (literal "<"))
         (field right (reference expression)))))
      (alias NumberExpression (field value (token number)))))))

(def grammar-composition-tests
  (test-suite "grammar composition"
    (test-case "normalization is deterministic"
      (check (grammar-ir-canonical (compile-grammar arithmetic-grammar))
             =>
             (grammar-ir-canonical (compile-grammar arithmetic-grammar))))
    (test-case "parser IR preserves declared flow"
      (check (parser-ir-ref arithmetic-parser-ir 'schema)
             => "gerbil-parser.parser-ir.v1")
      (check (parser-ir-ref arithmetic-parser-ir 'flow)
             => '((source lexical) (lexical expression) (expression cst))))
    (test-case "deterministic LALR(1) compilation admits declared precedence"
      (let (lr-spec (parser-ir-ref arithmetic-parser-ir 'lr-spec))
        (check (cdr (assq 'schema lr-spec))
               => "gerbil-parser.lr-spec.v1")
        (check (lr-spec-ref lr-spec 'algorithm)
               => 'lalr1-lr0-fixed-point-v1)
        (check (lr-spec-ref lr-spec 'lr0-state-visit-count)
               => (lr-spec-ref lr-spec 'state-count))
        (check (> (lr-spec-ref lr-spec 'lookahead-item-visit-count) 0)
               => #t)
        (check (> (cdr (assq 'state-count lr-spec)) 0) => #t)))
    (test-case "language declarations materialize LR tables during AOT expansion"
      (check (parser-ir-ref arithmetic-parser-ir 'materialization)
             => 'aot-expansion)
      (check (pair? (parser-ir-ref arithmetic-parser-ir 'lr-spec)) => #t))
    (test-case "canonical parser IR including LALR tables is deterministic"
      (check (parser-ir-canonical (compile-parser arithmetic-grammar))
             =>
             (parser-ir-canonical (compile-parser arithmetic-grammar))))
    (test-case "grammar algebra preserves productions and entrypoint"
      (check (parser-ir-ref arithmetic-parser-ir 'root-rule) => 'source-file)
      (check (length (parser-ir-ref arithmetic-parser-ir 'terminals)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'lexical-rules)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'rules)) => 6)
      (check (parser-ir-ref arithmetic-parser-ir 'extras)
             => '((whitespace)))
      (let* ((rules (parser-ir-ref arithmetic-parser-ir 'rules))
             (source-expression (cadr (assq 'source-file rules))))
        (check (grammar-expression-kind source-expression) => 'alias)
        (check source-expression
               => '(alias SourceFile
                     (field expression (reference expression))))))
    (test-case "nullable repetition is rejected at construction"
      (check-exception
       (grammar-expression
        (repeat (optional (token identifier))))
       true))
    (test-case "lexical literals reject empty spellings"
      (check-exception
       (lexical-expression (literals ""))
       true))
    (test-case "every terminal requires exactly one lexical rule"
      (check-exception
       (compile-composed missing-lexical-rule-grammar)
       true))
    (test-case "unresolved production references are rejected"
      (check-exception
       (compile-composed unresolved-reference-grammar)
       true))
    (test-case "terminals require token syntax kinds"
      (check-exception
       (compile-composed invalid-terminal-kind-grammar)
       true))
    (test-case "precedence-bearing grammars compile to the LR owner"
      (check (cdr (assq 'schema
                        (parser-ir-ref
                         (compile-composed unsupported-precedence-grammar)
                         'lr-spec)))
             => "gerbil-parser.lr-spec.v1"))
    (test-case "unresolved LR ambiguity fails closed"
      (check (condition-message
              (lambda ()
                (compile-lr-spec ambiguous-left-recursive-rules
                                 'source-file)))
             => "unresolved shift/reduce conflict requires precedence"))
    (test-case "selective GLR commits the only successful session branch"
      (let (spec
            (compile-lr-spec selective-session-rules
                             'source-file 'selective-glr))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "CLOSE" 20 25)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "SET" 20 23)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))))
    (test-case "dynamic precedence requires selective GLR admission"
      (check (condition-message
              (lambda ()
                (compile-lr-spec dynamic-precedence-rules 'source-file)))
             => "dynamic precedence requires selective GLR admission"))
    (test-case "non-associative precedence rejects only chained operators"
      (let (spec (compile-lr-spec nonassociative-rules 'source-file))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'number "1" 0 1)
                             (make-token 'punctuation "<" 1 2)
                             (make-token 'number "2" 2 3)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))
        (check-exception
         (lr-parse
          spec
          (list (make-token 'number "1" 0 1)
                (make-token 'punctuation "<" 1 2)
                (make-token 'number "2" 2 3)
                (make-token 'punctuation "<" 3 4)
                (make-token 'number "3" 4 5)))
         true)))
    (test-case "same lexical identity with a different definition is rejected"
      (check-exception
       (compile-grammar conflicting-arithmetic-grammar)
       true))
    (test-case "the generic machine executes the declared entrypoint"
      (let-values (((root rest)
                    ((parser-machine-parse arithmetic-parser)
                     (list (make-token 'number "1" 0 1)))))
        (check (recognition-node-kind root) => 'SourceFile)
        (check rest => '())))
    (test-case "extras generate the sole trivia predicate"
      (check ((parser-machine-trivia arithmetic-parser)
              (make-token 'whitespace " " 0 1))
             => '(whitespace))
      (check ((parser-machine-trivia arithmetic-parser)
              (make-token 'punctuation "+" 0 1))
             => #f))))

(run-tests! grammar-composition-tests)
