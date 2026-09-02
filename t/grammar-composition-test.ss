#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/modules/parser/objects
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/compiler/generate
        :gerbil-parser/src/reference/arithmetic)

(defgrammar-role conflicting-operator-role
  (syntax-kinds)
  (keywords)
  (prefix-operators ((punctuation "+") 99 left))
  (binary-operators)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar conflicting-arithmetic-grammar
  (supers arithmetic-grammar)
  (roles conflicting-operator-role))

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
