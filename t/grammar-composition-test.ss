#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/grammar/algebra
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/modules/parser/objects
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/compiler/generate
        :gerbil-parser/src/reference/arithmetic)

(defgrammar-role conflicting-operator-role
  (syntax-kinds)
  (terminals)
  (rules)
  (extras)
  (keywords)
  (prefix-operators ((punctuation "+") 99 left))
  (binary-operators)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar conflicting-arithmetic-grammar
  (supers arithmetic-grammar)
  (roles conflicting-operator-role))

(defgrammar-role unresolved-reference-role
  (syntax-kinds
   (SourceFile node ()))
  (terminals)
  (rules
   (source-file (reference missing-rule)))
  (extras)
  (keywords)
  (prefix-operators)
  (binary-operators)
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
  (rules
   (source-file
    (alias SourceFile (field value (token value)))))
  (extras)
  (keywords)
  (prefix-operators)
  (binary-operators)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar invalid-terminal-kind-grammar
  (supers)
  (roles invalid-terminal-kind-role))

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
    (test-case "operator configuration generated canonical IR"
      (check (length (parser-ir-ref arithmetic-parser-ir 'binary-operators)) => 4)
      (check (length (parser-ir-ref arithmetic-parser-ir 'prefix-operators)) => 2))
    (test-case "grammar algebra preserves productions and entrypoint"
      (check (parser-ir-ref arithmetic-parser-ir 'root-rule) => 'source-file)
      (check (length (parser-ir-ref arithmetic-parser-ir 'terminals)) => 4)
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
    (test-case "unresolved production references are rejected"
      (check-exception
       (compile-parser unresolved-reference-grammar)
       true))
    (test-case "terminals require token syntax kinds"
      (check-exception
       (compile-parser invalid-terminal-kind-grammar)
       true))
    (test-case "same operator identity with a different definition is rejected"
      (check-exception
       (compile-grammar conflicting-arithmetic-grammar)
       true))
    (test-case "operator lookup is generated as direct machine behavior"
      (check ((parser-machine-prefix arithmetic-parser)
              (make-token 'punctuation "+" 0 1))
             => '(punctuation "+" 30 right))
      (check ((parser-machine-binary arithmetic-parser)
              (make-token 'identifier "value" 0 5))
             => #f))))

(run-tests! grammar-composition-tests)
