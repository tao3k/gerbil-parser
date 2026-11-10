(import :gerbil-parser/language-test-support)
;;; Consume the installed public owner; tests do not compile their own copy.
(import (only-in :gerbil-parser/source-line-support fold-source-input-bytes))

(deflanguage-parser-tests source-input-test "Compiled source input admission"
  (engine)
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
