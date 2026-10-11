;;; -*- Gerbil -*-
;;; Variable-width delimiters have one canonical Scheme lexical boundary.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/machine lexical-end)
        (only-in :gerbil-parser/src/grammar/lexical-algebra
                 lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-character-run))
(export lexical-character-run-test)

(def (scan-hyphens source offset)
  (lexical-end source offset (character-run "-" 4)))

(def lexical-character-run-test
  (test-suite "character-run lexical primitive"
    (test-case "canonical algebra requires one character and a positive minimum"
      (check (lexical-expression? '(character-run "-" 4)) => #t)
      (check (lexical-expression? '(character-run "é" 2)) => #t)
      (check (lexical-expression? '(character-run "--" 4)) => #f)
      (check (lexical-expression? '(character-run "-" 0)) => #f))
    (test-case "generated scanner consumes a maximal qualified run"
      (check (scan-hyphens "---- MODULE" 0) => 4)
      (check (scan-hyphens "------- MODULE" 0) => 7)
      (check (scan-hyphens "--- MODULE" 0) => #f)
      (check (scan-hyphens "x----" 1) => 5)
      (check (scan-character-run "ééx" 0 "é" 2) => 2)
      (check (scan-character-run "----" 0 "" 2) => #f)
      (check (scan-character-run "----" 0 "-" 0) => #f))))
