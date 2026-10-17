(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Variable-width delimiters have one canonical Scheme lexical boundary.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/machine lexical-end)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-character-run))


(def (scan-hyphens source offset)
  (lexical-end source offset (character-run "-" 4)))

(deflanguage-parser-tests lexical-character-run-test "character-run lexical primitive"
  (engine)
    (property "canonical algebra requires one character and a positive minimum"
      (bindings)
      (equal (lexical-expression? '(character-run "-" 4))  #t)
      (equal (lexical-expression? '(character-run "é" 2))  #t)
      (equal (lexical-expression? '(character-run "--" 4))  #f)
      (equal (lexical-expression? '(character-run "-" 0))  #f))
    (property "generated scanner consumes a maximal qualified run"
      (bindings)
      (equal (scan-hyphens "---- MODULE" 0)  4)
      (equal (scan-hyphens "------- MODULE" 0)  7)
      (equal (scan-hyphens "--- MODULE" 0)  #f)
      (equal (scan-hyphens "x----" 1)  5)
      (equal (scan-character-run "ééx" 0 "é" 2)  2)
      (equal (scan-character-run "----" 0 "" 2)  #f)
      (equal (scan-character-run "----" 0 "-" 0)  #f)))
