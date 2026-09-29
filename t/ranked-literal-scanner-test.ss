#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Shared literal trie preserves generated lexer's longest/rank/order contract.

(import :std/test
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-literal-scanner))

(def ranked-literal-scanner-tests
  (test-suite "ranked literal scanner"
    (test-case "longest match precedes rank, then declaration order"
      (let (scan
            (make-ranked-literal-scanner
             '(("a" short 99 0)
               ("ab" first 2 4)
               ("ab" later 2 5)
               ("ab" higher 3 6)
               ("α" unicode 0 7))))
        (check (scan "ab!" 0) => '(higher 2 3 6))
        (check (scan "a!" 0) => '(short 1 99 0))
        (check (scan "!α" 1) => '(unicode 2 0 7))
        (let (admitted (make-vector 8 #f))
          (vector-set! admitted 0 #t)
          (vector-set! admitted 4 #t)
          (check (scan "ab!" 0 admitted) => '(first 2 2 4))
          (vector-set! admitted 4 #f)
          (check (scan "ab!" 0 admitted) => '(short 1 99 0)))
        (check (scan "!" 0) => #f)))
    (test-case "equal rank retains the earlier rule"
      (let (scan
            (make-ranked-literal-scanner
             '(("word" first 1 3)
               ("word" second 1 4))))
        (check (scan "word!" 0) => '(first 4 1 3))))))
