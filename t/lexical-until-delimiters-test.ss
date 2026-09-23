;;; -*- Gerbil -*-
;;; Generic AOT atom boundary shared by Scheme and Rust consumers.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan
                 scan-escaped-quoted-strings scan-until-delimiters))
(export lexical-until-delimiters-test)

(def lexical-until-delimiters-test
  (test-suite "bounded lexical atom"
    (test-case "the canonical algebra admits one nonempty delimiter set"
      (check (lexical-expression? '(until-delimiters " \t\r\n();\"")) => #t)
      (check (lexical-expression? '(until-delimiters "")) => #f))
    (test-case "source characters stop before punctuation without allocation"
      (check (scan-until-delimiters ":scope) tail" 0 " \t\r\n();\"") => 6)
      (check (scan-until-delimiters "π-link; note" 0 " \t\r\n();\"") => 6)
      (check (scan-until-delimiters ")" 0 " \t\r\n();\"") => #f))
    (test-case "escaped quoted strings do not absorb their neighbor"
      (check (lexical-expression? '(escaped-quoted-string "\"")) => #t)
      (check (scan-escaped-quoted-strings "\"a\"\"b\"" 0 '("\"")) => 3))))
