(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; A whole-line lexical primitive has identical character boundaries to Rust.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-line))


(deflanguage-parser-tests lexical-line-test "whole-line lexical primitive"
  (engine)
    (property "canonical lexical algebra admits exactly one line primitive"
      (bindings)
      (equal (lexical-expression? '(line))  #t)
      (equal (lexical-expression? '(line "unexpected"))  #f))
    (property "UTF-8 and each line ending stay lossless"
      (bindings)
      (equal (scan-line "é\r\nnext" 0)  3)
      (equal (scan-line "é\r\nnext" 3)  7)
      (equal (scan-line "one\ntwo" 0)  4)
      (equal (scan-line "one\rtwo" 0)  4)
      (equal (scan-line "last" 0)  4)
      (equal (scan-line "" 0)  #f)))
