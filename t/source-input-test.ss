(import :gerbil-parser/language-test-support)
;;; Consume the installed public owner; tests do not compile their own copy.
(import (only-in :gerbil-parser/source-line-support
                 fold-source-input-bytes source-line-end fold-source-byte-spans))

;; Independent byte-level oracle includes arbitrary offsets, including the
;; second byte of CRLF and offsets inside a Unicode scalar.
(def (reference-line-end bytes start)
  (let (size (u8vector-length bytes))
    (let loop ((cursor start))
      (cond
       ((= cursor size) cursor)
       ((= (u8vector-ref bytes cursor) 10) (+ cursor 1))
       ((= (u8vector-ref bytes cursor) 13)
        (+ cursor (if (and (< (+ cursor 1) size)
                           (= (u8vector-ref bytes (+ cursor 1)) 10)) 2 1)))
       (else (loop (+ cursor 1)))))))

(deflanguage-parser-tests source-input-test "Compiled source input admission"
  (engine)
    (scenario "shared line scan preserves all byte offsets and terminators"
      (each (text) ('("" "ascii" "\n" "\r" "\r\n" "a\rb\nc\r\nd"
                     "é中🦀\r\nx\ry\nz" "\r\r\n\n"))
        (sequential ((bytes (string->utf8 text))
                     (before (u8vector->list bytes)))
          (each (start) ((iota (+ 1 (u8vector-length bytes))))
            (equal (source-line-end bytes start)
                   (reference-line-end bytes start)))
          (equal (fold-source-byte-spans bytes 0
                   (lambda (from until previous)
                     (unless (and (= from previous)
                                  (= until (source-line-end bytes from)))
                       (error "source traversal changed line boundaries"))
                     until))
                 (u8vector-length bytes))
          (equal (u8vector->list bytes) before))))
    (scenario "foreign line scan rejects invalid storage and offsets"
      (each (invalid) ('(#f 42 () "text"))
        (raises (source-line-end invalid 0) true))
      (each (invalid) ('(-1 2 1/2 1.0 #f 18446744073709551616))
        (raises (source-line-end #u8(10) invalid) true)))
    (scenario "compiled byte input rejects malformed UTF-8 before traversal"
      (each (byte) ((iota 128 128))
        (raises (fold-source-input-bytes (u8vector byte)) true))
      (each (invalid) ((list #u8(255) #u8(192 128) #u8(237 160 128)
                             #u8(244 144 128 128) #u8(226 130) #u8(128)))
        (raises (fold-source-input-bytes invalid) true)))
    (scenario "valid input preserves scalar text and the caller-owned byte buffer"
      (each (text) ('("" "ascii" "é中🦀\r\nx\ry\nz"))
        (sequential ((bytes (string->utf8 text))
                     (before (u8vector->list bytes)))
          (equal (eq? (fold-source-input-bytes bytes) bytes) #t)
          (equal (u8vector->list bytes) before)
          (equal (fold-source-input-bytes text) bytes))))
    (scenario "non-source values remain outside the input contract"
      (each (invalid) ('(42 #f () source))
        (raises (fold-source-input-bytes invalid) true))))
