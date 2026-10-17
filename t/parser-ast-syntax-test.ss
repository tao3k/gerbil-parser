(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Canonical parser tests assert exact AST, fields, spans and source bytes.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 parse-arithmetic)
        (only-in :gerbil-parser/src/runtime/cst parse-artifact->cst)
        (only-in :gerbil-parser/src/testing/parser-ast
                 check-parser-ast check-accepted-parse
                 parser-ast-diff parser-ast-pattern
                 parser-artifact-ast-diff))


(deflanguage-parser-tests parser-ast-syntax-test "parser-owned AST test syntax"
  (engine)
    (scenario "accepted artifact macro is hygienic and evaluates once"
      (scope ((result "1")
            (expected-source 'caller-binding)
            (source-calls 0)
            (candidate-calls 0))
        (accepted-parse
         (begin (set! source-calls (fx+ source-calls 1)) result)
         (begin
           (set! candidate-calls (fx+ candidate-calls 1))
           (parse-arithmetic result))
         (parse-arithmetic result))
        (equal source-calls  1)
        (equal candidate-calls  1)
        (equal expected-source  'caller-binding)))
    (scenario "one source proves exact kinds, fields, spans, token and roundtrip"
      (ast (parse-arithmetic "1") "1"
        (node SourceFile 0 1
          (field expression 0 1
            (node NumberExpression 0 1
              (field value 0 1
                (token number "1" 0 1)))))))
    (property "a wrong field cannot pass as a matching source roundtrip"
      (bindings (actual (parse-artifact->cst (parse-arithmetic "1"))))
      (equal (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 1
                   (field wrong 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "1" 0 1)))))))
                '((0) kind wrong expression)))
    (property "byte span and token text differences name the failed leaf"
      (bindings (actual (parse-artifact->cst (parse-arithmetic "1"))))
      (equal (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 1
                   (field expression 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "2" 0 1)))))))
                '((0 0 0 0) lexeme "2" "1"))
      (equal (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 2
                   (field expression 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "1" 0 1)))))))
                '(() end 2 1)))
    (property "source mismatch is part of the same AST acceptance contract"
      (bindings)
      (equal (parser-artifact-ast-diff
              (parse-arithmetic "1") "2"
              (parser-ast-pattern
               (node SourceFile 0 1
                 (field expression 0 1
                   (node NumberExpression 0 1
                     (field value 0 1
                       (token number "1" 0 1)))))))
              '(() source "2" "1"))))
