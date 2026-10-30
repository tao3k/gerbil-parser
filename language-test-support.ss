;;; -*- Gerbil -*-
;;; Public constrained language-test syntax; test-only dependency surface.
(import ./src/testing/language-parser)
(export deflanguage-parser-tests defparser-check parser-check parser-check-result parser-check-procedure parser-test-datum-writer)

(import (only-in ./src/testing/parser-ast check-parser-ast))
(export check-parser-ast)
