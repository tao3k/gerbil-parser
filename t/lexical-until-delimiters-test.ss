(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Generic AOT atom boundary shared by Scheme and Rust consumers.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan
                 scan-escaped-quoted-strings scan-until-delimiters))


(deflanguage-parser-tests lexical-until-delimiters-test "bounded lexical atom"
  (engine)
    (scenario "the canonical algebra admits one nonempty delimiter set"
      (equal (lexical-expression? '(until-delimiters " \t\r\n();\""))  #t)
      (equal (lexical-expression? '(until-delimiters ""))  #f))
    (scenario "source characters stop before punctuation without allocation"
      (equal (scan-until-delimiters ":scope) tail" 0 " \t\r\n();\"")  6)
      (equal (scan-until-delimiters "π-link; note" 0 " \t\r\n();\"")  6)
      (equal (scan-until-delimiters "π next" 0 "();\"")  1)
      (equal (scan-until-delimiters ")" 0 " \t\r\n();\"")  #f))
    (property "escaped quoted strings do not absorb their neighbor"
      (bindings)
      (equal (lexical-expression? '(escaped-quoted-string "\""))  #t)
      (equal (scan-escaped-quoted-strings "\"a\"\"b\"" 0 '("\""))  3)))
