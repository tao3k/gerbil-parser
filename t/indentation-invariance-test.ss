#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Non-semantic whitespace may change source/trivia spans, never syntax.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/cypher/opencypher-2024-1/parser
        :gerbil-parser/languages/gql/iso-39075-2024/parser
        :gerbil-parser/languages/hcl/v2-24/parser
        :gerbil-parser/languages/tla-plus/v1/parser
        (only-in :gerbil-parser/src/compiler/machine parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/token
                 token? token-kind token-lexeme))

;;; Project one lossless CST to its location-independent native syntax shape.
;;; Trivia is intentionally absent; significant token kinds and lexemes remain,
;;; so this proves more than acceptance or a shallow node-kind comparison.
;; : (-> ParserMachine CSTValue Datum)
(def (native-syntax-shape machine value)
  (cond
   ((syntax-node? value)
    (cons 'node
          (cons (syntax-node-kind value)
                (filter values
                        (map (cut native-syntax-shape machine <>)
                             (syntax-node-children value))))))
   ((syntax-field? value)
    (cons 'field
          (cons (syntax-field-name value)
                (filter values
                        (map (cut native-syntax-shape machine <>)
                             (syntax-field-children value))))))
   ((token? value)
    (if ((parser-machine-trivia machine) value)
      #f
      (list 'token (token-kind value) (token-lexeme value))))
   (else
    (error "unexpected CST value" value))))

;; : (-> ParserMachine Procedure String String Void)
(def (check-indentation-invariance machine parse plain indented)
  (let ((plain-artifact (parse plain))
        (indented-artifact (parse indented)))
    (check (parse-artifact-success? plain-artifact) => #t)
    (check (parse-artifact-success? indented-artifact) => #t)
    (check (native-syntax-shape machine
                                (parse-artifact->cst indented-artifact))
           =>
           (native-syntax-shape machine
                                (parse-artifact->cst plain-artifact)))))

(def indentation-invariance-tests
  (test-suite "native AST indentation invariance"
    (test-case "non-semantic indentation preserves native syntax shape"
      (check-indentation-invariance
       opencypher-2024-1-parser parse-opencypher-2024-1
       "MATCH (n)\nRETURN n\n"
       "  MATCH   (n)\n      RETURN   n\n")
      (check-indentation-invariance
       gql-iso-parser parse-gql-iso-39075-2024
       "MATCH (n)\nRETURN n\n"
       "  MATCH   (n)\n      RETURN   n\n")
      (check-indentation-invariance
       hcl-v2-24-parser parse-hcl-v2-24
       "resource \"x\" \"y\" {\nvalue = 1\n}\n"
       "resource   \"x\"   \"y\" {\n      value   =   1\n}\n")
      (check-indentation-invariance
       tla-plus-v1-parser parse-tla-plus-v1
       "---- MODULE Indent ----\nVARIABLE x\nInit == x = 1\n====\n"
       "---- MODULE Indent ----\n    VARIABLE   x\n        Init   ==   x   =   1\n====\n"))))

(export indentation-invariance-tests)
