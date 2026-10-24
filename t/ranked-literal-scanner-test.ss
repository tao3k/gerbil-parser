#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Shared literal trie preserves generated lexer's longest/rank/order contract.

(import :std/test
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-literal-scanner make-bounded-literal-end-scanner
                 make-literal-trie literal-trie-child))

;;; Independent linear catalog oracle; no trie or candidate publication reuse.
(def (literal-reference entries source start admitted)
  (let loop ((rest entries) (best #f))
    (if (null? rest)
      (and best (list (cadr best) (+ start (string-length (car best))) (caddr best) (cadddr best)))
      (let* ((entry (car rest)) (text (car entry)) (width (string-length text)))
        (loop (cdr rest)
          (if (and (or (not admitted) (vector-ref admitted (cadddr entry)))
                   (<= (+ start width) (string-length source))
                   (string=? text (substring source start (+ start width)))
                   (or (not best) (> width (string-length (car best)))
                       (and (= width (string-length (car best)))
                            (or (> (caddr entry) (caddr best))
                                (and (= (caddr entry) (caddr best)) (< (cadddr entry) (cadddr best)))))))
            entry best))))))

(deflanguage-parser-tests ranked-literal-scanner-tests "ranked literal scanner"
  (engine)
    (scenario "longest match precedes rank, then declaration order"
      (scope ((scan
            (make-ranked-literal-scanner
             '(("a" short 99 0)
               ("ab" first 2 4)
               ("ab" later 2 5)
               ("ab" higher 3 6)
               ("α" unicode 0 7)))))
        (equal (scan "ab!" 0)  '(higher 2 3 6))
        (equal (scan "a!" 0)  '(short 1 99 0))
        (equal (scan "!α" 1)  '(unicode 2 0 7))
        (scope ((admitted (make-vector 8 #f)))
          (act (vector-set! admitted 0 #t))
          (act (vector-set! admitted 4 #t))
          (equal (scan "ab!" 0 admitted)  '(first 2 2 4))
          (act (vector-set! admitted 4 #f))
          (equal (scan "ab!" 0 admitted)  '(short 1 99 0)))
        (equal (scan "!" 0)  #f)))
    (scenario "accepting prefixes agree with linear catalog under every mode mask"
      (sequential ((entries '(("a" short 9 0) ("ab" medium 1 1) ("abc" long 0 2)
                        ("ab" ranked 2 3) ("α" wide 1 4) ("α😀" wider 0 5)))
             (scan (make-ranked-literal-scanner entries)))
        (each (source) ('("" "a" "ab" "abc" "abcd" "!abc!α😀?" "α" "α😀" "x"))
          (iterate offsets ((at 0))
            (when (<= at (string-length source))
              (equal (scan source at)  (literal-reference entries source at #f))
              (iterate masks ((mask 0))
                (when (< mask 64)
                  (scope ((admitted (list->vector (map (lambda (index) (odd? (quotient mask (expt 2 index)))) (iota 6)))))
                    (equal (scan source at admitted)  (literal-reference entries source at admitted)))
                  (continue masks (+ mask 1))))
              (continue offsets (+ at 1)))))))
    (scenario "prepared widths and fresh results are isolated from caller mutation"
      (sequential ((text (string-copy "ab"))
             (scan (make-ranked-literal-scanner (list (list text 'word 1 0))))
             (result (scan "ab!" 0)))
        (act (string-set! text 0 #\x))
        (act (set-car! result 'foreign))
        (act (set-car! (cdr result) 999))
        (equal (scan "ab!" 0)  '(word 2 1 0))
        (equal (scan "xab" 1)  '(word 3 1 0))
        (equal (scan "xb" 0)  #f)))
    (scenario "rank catalogs retain filtered winners and fresh terminal publication"
      (sequential ((entries (map (lambda (rank) (list "word" rank rank rank)) (iota 16)))
             (scan (make-ranked-literal-scanner entries))
             (mask (make-vector 16 #f)))
        (equal (scan "word!" 0)  '(15 4 15 15))
        (each (rank) ((iota 16))
          (act (vector-set! mask rank #t))
          (equal (scan "word!" 0 mask)  (list rank 4 rank rank))
          (act (vector-set! mask rank #f)))
        (equal (scan "word!" 0 mask)  #f)
        (scope ((result (scan "word!" 0)))
          (act (set-car! (cddr result) -100))
          (equal (scan "word!" 0)  '(15 4 15 15)))))
    (scenario "dense root directory retains every ASCII edge and Unicode fallback"
      (sequential ((entries (append (map (lambda (code)
                                    (list (string (integer->char code)) 'branch 0 (- code 32))) (iota 96 32))
                             '(("λ😀" unicode 0 96))))
             (scan (make-ranked-literal-scanner entries))
             (mask (make-vector 97 #t)))
        (each (source) ((append (map (lambda (code) (string (integer->char code))) (iota 128))
                  '("!λ😀?" "λ" "λ😀" "")))
          (iterate offsets ((start 0))
            (when (<= start (string-length source))
              (equal (scan source start)  (literal-reference entries source start #f))
              (equal (scan source start mask)  (literal-reference entries source start mask))
              (continue offsets (+ start 1)))))
        (act (vector-set! mask 96 #f))
        (equal (scan "λ😀" 0 mask)  #f)))
    (property "deep prepared paths preserve bounded endpoints without preparation stack growth"
      (bindings (text (make-string 8192 #\a)) (scan (make-bounded-literal-end-scanner (list text "a" "β😀"))))
      (equal (scan text 0 8192)  8192)
      (equal (scan text 0 8191)  1)
      (equal (scan "!β😀!" 1 3)  3)
      (equal (scan "!β😀!" 1 2)  #f)
      (equal (scan text -1 8192)  #f)
      (equal (scan text 0 8193)  #f))
    (scenario "child access evaluates supplied node and character once"
      (scope ((root (make-literal-trie '(("ab" . terminal)))) (calls 0))
        (equal (not (not (literal-trie-child
                          (begin (set! calls (+ calls 1)) root)
                          (begin (set! calls (+ calls 1)) #\a))))  #t)
        (equal calls  2)
        (equal (literal-trie-child root #\z)  #f)))
    (property "equal rank retains the earlier rule"
      (bindings (scan
            (make-ranked-literal-scanner
             '(("word" first 1 3)
               ("word" second 1 4)))))
      (equal (scan "word!" 0)  '(first 4 1 3))))

;; gxtest discovers only exported names ending in -test.
(def ranked-literal-scanner-test ranked-literal-scanner-tests)
(export ranked-literal-scanner-test)
