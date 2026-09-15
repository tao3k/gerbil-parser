#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/grammar/algebra
        :gerbil-parser/src/grammar/lexical-algebra
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/compiler/lr
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        :gerbil-parser/src/language/grammar
        :gerbil-parser/src/modules/parser/interface
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/funcs vector-intern-map)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-checkpoint? lr-checkpoint-deterministic-actions
                 lr-checkpoint-deterministic-shifts
                 lr-checkpoint-remaining-token-count
                 lr-checkpoint-advance lr-checkpoint-resume
                 lr-initial-checkpoint lr-parse lr-parse/receipt lr-prepare)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/languages/arithmetic/v1/parser)

;;; Both rules recognize the same spelling. Only the LR state can select the
;;; correct token identity for each position; global longest-match necessarily
;;; selects the declaration-first identity twice.
(deflanguage directed-lexical-mode
  (identity "directed-lexical-mode" "v1" "directed-lexical-mode.v1")
  (root source-file)
  (lex
   (first FirstToken (literals "x"))
   (second SecondToken (literals "x")))
  (rules
   (source-file
    (node SourceFile
      (seq (field first first) (field second second)))))
  (extras)
  (keywords)
  (recoveries)
  (conflicts reject)
  (case-insensitive #f))

(defgrammar-role base-lexical-role
  (syntax-kinds
   (Punctuation token (text)))
  (terminals
   (punctuation Punctuation))
  (lexical-rules
   (punctuation (literals "+" "-" "*" "**" "/" "(" ")")))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar base-object-grammar
  (supers)
  (roles base-lexical-role))

(defgrammar-role appended-identifier-role
  (syntax-kinds (Identifier token (text)))
  (terminals (identifier Identifier))
  (lexical-rules (identifier (identifier)))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar-compose explicit-composed-grammar
  (supers base-object-grammar)
  (compose (append appended-identifier-role)))

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
  (supers base-object-grammar)
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

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (compile-composed grammar)
  (compile-parser (compile-grammar grammar)))

(def dynamic-precedence-rules
  '((source-file
     (precedence dynamic 1
      (alias SourceFile (field value (token identifier)))))))

(def competing-dynamic-precedence-rules
  '((source-file
     (choice (reference low-path) (reference high-path)))
    (low-path
     (precedence dynamic 1
      (alias LowPath (field value (token identifier)))))
    (high-path
     (precedence dynamic 2
      (alias HighPath (field value (token identifier)))))))

;;; The two reductions accept the same token and publish the same tree.  A
;;; correct selective-GLR runtime must execute both and merge their completed
;;; candidates even when neither production carries dynamic precedence.
(def static-equivalent-ambiguity-rules
  '((source-file
     (choice (reference first-path) (reference second-path)))
    (first-path
     (alias SourceFile (field value (token identifier))))
    (second-path
     (alias SourceFile (field value (token identifier))))))

;;; The same reduce/reduce shape deliberately publishes distinct roots.  The
;;; runtime must not turn declaration order into an implicit ambiguity policy.
(def static-distinct-ambiguity-rules
  '((source-file
     (choice (reference first-path) (reference second-path)))
    (first-path
     (alias FirstPath (field value (token identifier))))
    (second-path
     (alias SecondPath (field value (token identifier))))))

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
    (test-case "POO contracts own grammar values and section dispatch"
      (check (grammar-role? base-lexical-role) => #t)
      (check (grammar? base-object-grammar) => #t)
      (check (grammar-role-name base-lexical-role) => 'base-lexical-role)
      (check (grammar-role-ref base-lexical-role 'terminals)
             => '((punctuation Punctuation)))
      (check (row-ref (grammar->alist base-object-grammar) 'schema)
             => "gerbil-parser.grammar.v1")
      (check-exception
       (make-grammar 'projection-is-not-a-parent
                     (list arithmetic-grammar)
                     (list base-lexical-role))
       true))
    (test-case "normalization is deterministic"
      (check (grammar-ir-canonical (compile-grammar arithmetic-grammar))
             =>
             (grammar-ir-canonical (compile-grammar arithmetic-grammar))))
    (test-case "standard vector traversal interns projected catalogs"
      (let-values
          (((catalog canonical)
            (vector-intern-map
             '#(left right left right)
             (lambda (value) value)
             (lambda (key id) (cons id key)))))
        (check (vector-length canonical) => 2)
        (check (eq? (vector-ref catalog 0) (vector-ref catalog 2)) => #t)
        (check (eq? (vector-ref catalog 1) (vector-ref catalog 3)) => #t)))
    (test-case "LR states direct identical lexical spellings"
      (let ((global-tokens (lex-source directed-lexical-mode-parser "xx"))
            (artifact (parse-source directed-lexical-mode-parser "xx")))
        (check (map token-kind global-tokens) => '(first first))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => "xx")))
    (test-case "explicit POO composition emits an identity-bearing receipt"
      (let-values (((ir receipt)
                    (compile-grammar/receipt explicit-composed-grammar)))
        (check (grammar-ir-ref ir 'terminals)
               => '((punctuation Punctuation) (identifier Identifier)))
        (check (row-ref receipt 'schema)
               => "gerbil-parser.grammar-composition-receipt.v1")
        (check (grammar-ir-ref ir 'compositionDigest)
               => (row-ref receipt 'compositionDigest))
        (check (map (lambda (step) (row-ref step 'operation))
                    (row-ref receipt 'steps))
               => '(merge append))))
    (test-case "explicit append fails closed on an existing identity"
      (let (conflicting
            (make-grammar
             'conflicting-explicit-append
             (list base-object-grammar)
             '()
             (list (cons 'append base-lexical-role))))
        (check (condition-message (lambda () (compile-grammar conflicting)))
               => "grammar append target already exists")))
    (test-case "parser IR preserves declared flow"
      (check (parser-ir-ref arithmetic-parser-ir 'schema)
             => "gerbil-parser.parser-ir.v1")
      (check (parser-ir-ref arithmetic-parser-ir 'flow)
             => '((source lexical) (lexical parser) (parser cst))))
    (test-case "deterministic LALR(1) compilation admits declared precedence"
      (let (lr-spec (parser-ir-ref arithmetic-parser-ir 'lr-spec))
        (check (lr-spec-ref lr-spec 'schema)
               => "gerbil-parser.lr-spec.v1")
        (check (lr-spec-ref lr-spec 'algorithm)
               => 'lalr1-lr0-fixed-point-v1)
        (check (lr-spec-ref lr-spec 'lr0-state-visit-count)
               => (lr-spec-ref lr-spec 'state-count))
        (check (> (lr-spec-ref lr-spec 'lookahead-item-visit-count) 0)
               => #t)
        (check (> (lr-spec-ref lr-spec 'state-count) 0) => #t)))
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
      (check (lr-spec-ref
              (parser-ir-ref
               (compile-composed unsupported-precedence-grammar)
               'lr-spec)
              'schema)
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
    (test-case "static selective GLR merges equivalent completed branches"
      (let (spec
            (compile-lr-spec static-equivalent-ambiguity-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '())
          (check (row-ref receipt 'branchesExplored) => 2)
          (check (row-ref receipt 'mergedBranches) => 1)
          (check (row-ref receipt 'ambiguousBranches) => 0))))
    (test-case "static selective GLR rejects distinct completed branches"
      (let (spec
            (compile-lr-spec static-distinct-ambiguity-rules
                             'source-file 'selective-glr))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (call-with-values
             (lambda ()
               (lr-parse/receipt
                spec (list (make-token 'identifier "x" 0 1))))
             (lambda _ #f))))
         => "selective GLR ambiguity is unresolved")))
    (test-case "dynamic precedence requires selective GLR admission"
      (check (condition-message
              (lambda ()
                (compile-lr-spec dynamic-precedence-rules 'source-file)))
             => "dynamic precedence requires selective GLR admission")
      (let (spec
            (compile-lr-spec dynamic-precedence-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '())
          (check (row-ref receipt 'dynamicScore) => 1)
          (check (row-ref receipt 'schema)
                 => "gerbil-parser.selective-glr-receipt.v1"))))
    (test-case "dynamic precedence ranks completed reduce branches"
      (check
       (condition-message
        (lambda ()
          (compile-lr-spec competing-dynamic-precedence-rules
                           'source-file)))
       => "dynamic precedence requires selective GLR admission")
      (let (spec
            (compile-lr-spec competing-dynamic-precedence-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'HighPath)
          (check rest => '())
          (check (row-ref receipt 'branchesExplored) => 2)
          (check (row-ref receipt 'successfulCompletions) => 2)
          (check (row-ref receipt 'distinctCompletions) => 2)
          (check (row-ref receipt 'winnerReason) => 'dynamic-precedence)
          (check (row-ref receipt 'dynamicScore) => 2))))
    (test-case "immutable checkpoints resume the sole LR executor"
      (let* ((spec (compile-lr-spec nonassociative-rules 'source-file))
             (runtime (lr-prepare spec))
             (tokens
              (list (make-token 'number "1" 0 1)
                    (make-token 'punctuation "<" 1 2)
                    (make-token 'number "2" 2 3)))
             (initial (lr-initial-checkpoint runtime tokens)))
        (check (lr-checkpoint? initial) => #t)
        (check (lr-checkpoint-deterministic-actions initial) => 0)
        (check (lr-checkpoint-remaining-token-count initial) => 3)
        (let-values (((status checkpoint) (lr-checkpoint-advance initial 1)))
          (check status => 'checkpoint)
          (check (lr-checkpoint-deterministic-actions checkpoint) => 1)
          (check (lr-checkpoint-deterministic-shifts checkpoint) => 1)
          (check (lr-checkpoint-remaining-token-count checkpoint) => 2)
          (let-values (((resumed-root resumed-rest)
                        (lr-checkpoint-resume checkpoint))
                       ((fresh-root fresh-rest) (lr-parse spec tokens)))
            (check resumed-root => fresh-root)
            (check resumed-rest => fresh-rest)))))
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
