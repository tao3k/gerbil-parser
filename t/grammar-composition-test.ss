#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/grammar/algebra
        :gerbil-parser/src/grammar/lexical-algebra
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
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
    (test-case "precedence is expressed by generic grammar levels"
      (let (rules (parser-ir-ref arithmetic-parser-ir 'rules))
        (check (grammar-expression-kind (cadr (assq 'expression rules)))
               => 'alias)
        (check (grammar-expression-kind (cadr (assq 'term rules)))
               => 'alias)))
    (test-case "grammar algebra preserves productions and entrypoint"
      (check (parser-ir-ref arithmetic-parser-ir 'root-rule) => 'source-file)
      (check (length (parser-ir-ref arithmetic-parser-ir 'terminals)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'lexical-rules)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'rules)) => 8)
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
       (compile-parser missing-lexical-rule-grammar)
       true))
    (test-case "unresolved production references are rejected"
      (check-exception
       (compile-parser unresolved-reference-grammar)
       true))
    (test-case "terminals require token syntax kinds"
      (check-exception
       (compile-parser invalid-terminal-kind-grammar)
       true))
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
