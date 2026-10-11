#!/usr/bin/env gxi
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
    (test-case "accepting prefixes agree with linear catalog under every mode mask"
      (let* ((entries '(("a" short 9 0) ("ab" medium 1 1) ("abc" long 0 2)
                        ("ab" ranked 2 3) ("α" wide 1 4) ("α😀" wider 0 5)))
             (scan (make-ranked-literal-scanner entries)))
        (for-each (lambda (source)
          (let offsets ((at 0))
            (when (<= at (string-length source))
              (check (scan source at) => (literal-reference entries source at #f))
              (let masks ((mask 0))
                (when (< mask 64)
                  (let (admitted (list->vector (map (lambda (index) (odd? (quotient mask (expt 2 index)))) (iota 6))))
                    (check (scan source at admitted) => (literal-reference entries source at admitted)))
                  (masks (+ mask 1))))
              (offsets (+ at 1)))))
          '("" "a" "ab" "abc" "abcd" "!abc!α😀?" "α" "α😀" "x"))))
    (test-case "prepared widths and fresh results are isolated from caller mutation"
      (let* ((text (string-copy "ab"))
             (scan (make-ranked-literal-scanner (list (list text 'word 1 0))))
             (result (scan "ab!" 0)))
        (string-set! text 0 #\x)
        (set-car! result 'foreign)
        (set-car! (cdr result) 999)
        (check (scan "ab!" 0) => '(word 2 1 0))
        (check (scan "xab" 1) => '(word 3 1 0))
        (check (scan "xb" 0) => #f)))
    (test-case "rank catalogs retain filtered winners and fresh terminal publication"
      (let* ((entries (map (lambda (rank) (list "word" rank rank rank)) (iota 16)))
             (scan (make-ranked-literal-scanner entries))
             (mask (make-vector 16 #f)))
        (check (scan "word!" 0) => '(15 4 15 15))
        (for-each (lambda (rank)
          (vector-set! mask rank #t)
          (check (scan "word!" 0 mask) => (list rank 4 rank rank))
          (vector-set! mask rank #f)) (iota 16))
        (check (scan "word!" 0 mask) => #f)
        (let (result (scan "word!" 0))
          (set-car! (cddr result) -100)
          (check (scan "word!" 0) => '(15 4 15 15)))))
    (test-case "dense root directory retains every ASCII edge and Unicode fallback"
      (let* ((entries (append (map (lambda (code)
                                    (list (string (integer->char code)) 'branch 0 (- code 32))) (iota 96 32))
                             '(("λ😀" unicode 0 96))))
             (scan (make-ranked-literal-scanner entries))
             (mask (make-vector 97 #t)))
        (for-each (lambda (source)
          (let offsets ((start 0))
            (when (<= start (string-length source))
              (check (scan source start) => (literal-reference entries source start #f))
              (check (scan source start mask) => (literal-reference entries source start mask))
              (offsets (+ start 1)))))
          (append (map (lambda (code) (string (integer->char code))) (iota 128))
                  '("!λ😀?" "λ" "λ😀" "")))
        (vector-set! mask 96 #f)
        (check (scan "λ😀" 0 mask) => #f)))
    (test-case "deep prepared paths preserve bounded endpoints without preparation stack growth"
      (let* ((text (make-string 8192 #\a))
             (scan (make-bounded-literal-end-scanner (list text "a" "β😀"))))
        (check (scan text 0 8192) => 8192)
        (check (scan text 0 8191) => 1)
        (check (scan "!β😀!" 1 3) => 3)
        (check (scan "!β😀!" 1 2) => #f)
        (check (scan text -1 8192) => #f)
        (check (scan text 0 8193) => #f)))
    (test-case "child access evaluates supplied node and character once"
      (let ((root (make-literal-trie '(("ab" . terminal)))) (calls 0))
        (check (not (not (literal-trie-child
                          (begin (set! calls (+ calls 1)) root)
                          (begin (set! calls (+ calls 1)) #\a)))) => #t)
        (check calls => 2)
        (check (literal-trie-child root #\z) => #f)))
    (test-case "equal rank retains the earlier rule"
      (let (scan
            (make-ranked-literal-scanner
             '(("word" first 1 3)
               ("word" second 1 4))))
        (check (scan "word!" 0) => '(first 4 1 3))))))

;; gxtest discovers only exported names ending in -test.
(def ranked-literal-scanner-test ranked-literal-scanner-tests)
(export ranked-literal-scanner-test)
