(import :std/test (only-in :std/vector/u8vector little u8vector-uint-set!)
        ../src/runtime/source-lines ../src/runtime/fold-tape)
(export fold-buffer-consumer-test)
(def fold-buffer-consumer-test
  (test-suite "Continuous source scan and fixed event records"
    (test-case "all record alignments preserve the full unsigned wire domain"
      (for-each
        (lambda (alignment)
          (let* ((header (make-u8vector alignment 77))
                 (owner (make-fold-tape header))
                 (reference (make-u8vector (+ alignment 23) 0)))
            (u8vector-copy! reference 0 header)
            (u8vector-set! reference alignment 1)
            (u8vector-uint-set! reference (+ alignment 1) 65535 little 2)
            (u8vector-set! reference (+ alignment 3) 2)
            (u8vector-uint-set! reference (+ alignment 4) 65535 little 2)
            (u8vector-uint-set! reference (+ alignment 6) 18446744073709551615 little 8)
            (u8vector-uint-set! reference (+ alignment 14) 9223372036854775808 little 8)
            (fold-tape-start! owner 65535)
            (fold-tape-token! owner 65535 18446744073709551615 9223372036854775808)
            (fold-tape-finish! owner)
            (check (fold-tape-publish! owner) => reference)))
        (iota 16)))
    (test-case "line offsets equal the character reference for mixed endings"
      (for-each
        (lambda (source)
          (let ((reference (fold-source-lines source '()
                             (lambda (_ from until rest) (cons (cons from until) rest))))
                (actual (fold-source-byte-spans (string->utf8 source) '()
                          (lambda (from until rest) (cons (cons from until) rest)))))
            (check actual => reference)))
        '("" "plain" "\r" "\n" "\r\n" "a\r\nb\rc\n" "é\r\nλ\rtail" "😀\n\r\nend")))
    (test-case "the scanner cannot precompute past a visitor mutation"
      (let* ((bytes (string->utf8 "a\nbc"))
             (spans (fold-source-byte-spans bytes '()
                      (lambda (from until rest)
                        (when (zero? from) (u8vector-set! bytes 2 10))
                        (cons (cons from until) rest)))))
        (check (reverse spans) => '((0 . 2) (2 . 3) (3 . 4)))))
    (test-case "visitor failure still stops traversal immediately"
      (let (calls 0)
        (check-exception
          (fold-source-byte-spans (string->utf8 "a\nb\n") #f
            (lambda (_from _until _state) (set! calls (+ calls 1)) (error "stop"))) true)
        (check calls => 1)))
    (test-case "publication transfers an exact isolated buffer including zero size"
      (let* ((header (u8vector 11 22 33)) (owner (make-fold-tape header))
             (published (fold-tape-publish! owner)))
        (u8vector-set! header 0 99)
        (check published => (u8vector 11 22 33))
        (check-exception (fold-tape-finish! owner) true)
        (check-exception (fold-tape-publish! owner) true))
      (let (owner (make-fold-tape (u8vector)))
        (check (fold-tape-publish! owner) => (u8vector))
        (check-exception (fold-tape-start! owner 0) true))
      ;; Transfer after repeated growth, then collect while the sealed owner
      ;; remains live. Publication must preserve exact bytes and stop writes.
      (let (owner (make-fold-tape (u8vector 77)))
        (for-each (lambda (index) (fold-tape-token! owner 9 index (+ index 1)))
                  (iota 100))
        (let* ((published (fold-tape-publish! owner))
               (snapshot (u8vector-copy published)))
          (##gc)
          (check (u8vector-length published) => 1901)
          (check published => snapshot)
          (check-exception (fold-tape-token! owner 9 0 1) true)
          (check-exception (fold-tape-publish! owner) true))))))
