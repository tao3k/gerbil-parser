;;; -*- Gerbil -*-
;;; Canonical parser tests assert exact AST, fields, spans and source bytes.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 parse-arithmetic-v1)
        (only-in :gerbil-parser/src/runtime/cst parse-artifact->cst)
        (only-in :gerbil-parser/src/testing/parser-ast
                 check-parser-ast parser-ast-diff parser-ast-pattern
                 parser-artifact-ast-diff))
(export parser-ast-syntax-test)

(def parser-ast-syntax-test
  (test-suite "parser-owned AST test syntax"
    (test-case "one source proves exact kinds, fields, spans, token and roundtrip"
      (check-parser-ast (parse-arithmetic-v1 "1") "1"
        (node SourceFile 0 1
          (field expression 0 1
            (node NumberExpression 0 1
              (field value 0 1
                (token number "1" 0 1)))))))
    (test-case "a wrong field cannot pass as a matching source roundtrip"
      (let (actual (parse-artifact->cst (parse-arithmetic-v1 "1")))
        (check (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 1
                   (field wrong 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "1" 0 1)))))))
               => '((0) kind wrong expression))))
    (test-case "byte span and token text differences name the failed leaf"
      (let (actual (parse-artifact->cst (parse-arithmetic-v1 "1")))
        (check (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 1
                   (field expression 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "2" 0 1)))))))
               => '((0 0 0 0) lexeme "2" "1"))
        (check (parser-ast-diff
                actual
                (parser-ast-pattern
                 (node SourceFile 0 2
                   (field expression 0 1
                     (node NumberExpression 0 1
                       (field value 0 1
                         (token number "1" 0 1)))))))
               => '(() end 2 1))))
    (test-case "source mismatch is part of the same AST acceptance contract"
      (check (parser-artifact-ast-diff
              (parse-arithmetic-v1 "1") "2"
              (parser-ast-pattern
               (node SourceFile 0 1
                 (field expression 0 1
                   (node NumberExpression 0 1
                     (field value 0 1
                       (token number "1" 0 1)))))))
             => '(() source "2" "1")))))
