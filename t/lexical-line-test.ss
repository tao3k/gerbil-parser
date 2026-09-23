;;; -*- Gerbil -*-
;;; A whole-line lexical primitive has identical character boundaries to Rust.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-line))
(export lexical-line-tests)

(def lexical-line-tests
  (test-suite "whole-line lexical primitive"
    (test-case "canonical lexical algebra admits exactly one line primitive"
      (check (lexical-expression? '(line)) => #t)
      (check (lexical-expression? '(line "unexpected")) => #f))
    (test-case "UTF-8 and each line ending stay lossless"
      (check (scan-line "é\r\nnext" 0) => 3)
      (check (scan-line "é\r\nnext" 3) => 7)
      (check (scan-line "one\ntwo" 0) => 4)
      (check (scan-line "one\rtwo" 0) => 4)
      (check (scan-line "last" 0) => 4)
      (check (scan-line "" 0) => #f))))
