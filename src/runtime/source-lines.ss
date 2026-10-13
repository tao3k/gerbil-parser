;;; -*- Gerbil -*-
;;; Request source traversal has no grammar/compiler initialization dependency.
(import (only-in :std/string/utf8 string-utf8-length utf8->string) :std/ffi)
(export fold-source-lines fold-source-byte-lines fold-source-byte-spans
        fold-source-frame-boundaries! fold-source-input-bytes source-line-events line-starts-with?)

;; The visitor still runs once for each original line, in order. Scan only
;; up to its next boundary before invoking it; no speculative scan past a
;; callback and no retained foreign pointer or new line-index allocation.
(C-ffi-macrology)
(C-include "<stdint.h>")
(def-C-lambda source-next-line-end (scheme-object unsigned-int64 unsigned-int64) unsigned-int64
  "const uint8_t *bytes = U8_DATA(___arg1);
   uint64_t cursor = ___arg2, size = ___arg3;
   while (cursor < size) {
     uint8_t byte = bytes[cursor++];
     if (byte == 10) break;
     if (byte == 13) {
       if (cursor < size && bytes[cursor] == 10) ++cursor;
       break;
     }
   }
   ___return(cursor);")

;; A call-scoped UTF-8 buffer can drive the same byte Fold without an
;; encode/decode/encode round trip. Retain official decoder admission: do not
;; reinterpret arbitrary bytes as valid text or weaken scalar checks.
;; The caller must not mutate the owned buffer during synchronous traversal;
;; no body pointer or mutable buffer is retained in published event cells.
(def (fold-source-input-bytes source)
  (cond
   ((string? source) (string->utf8 source))
   ((u8vector? source)
    (utf8->string source)
    source)
   (else (error "event Fold requires text or UTF-8 byte storage"))))

;; Generated native products already own the UTF-8 source. Traverse that
;; representation once, retaining each frame's bytes instead of encoding its
;; decoded line again. ASCII CR/LF cannot occur inside a UTF-8 continuation.
(def (fold-source-byte-lines (bytes : :u8vector) initial visit)
  (fold-source-byte-spans bytes initial
    (lambda (from until state)
      (let (line-bytes (subu8vector bytes from until))
        (visit (utf8->string line-bytes) line-bytes from until state)))))

;; Borrow the request buffer; no line byte slice or decoded string is created.
(def (fold-source-byte-spans (bytes : :u8vector) initial visit)
  (let (size (u8vector-length bytes))
    (let loop ((start 0) (state initial))
      (if (fx= start size) state
        (let (after (source-next-line-end bytes start size))
          (loop after (visit start after state)))))))

;; The source is string->utf8 output or checked UTF-8 input. A nonempty slice of that valid
;; buffer is valid UTF-8 iff both ends are character boundaries. Empty slices
;; remain valid even inside a codepoint, matching utf8->string of an empty view.
(def (fold-source-frame-boundaries! bytes from until)
  (unless (<= 0 from until (u8vector-length bytes))
    (error "native fold bounds outside source"))
  (unless (or (= from until)
              (and (not (<= 128 (u8vector-ref bytes from) 191))
                   (or (= until (u8vector-length bytes))
                       (not (<= 128 (u8vector-ref bytes until) 191)))))
    (error "native fold bounds split UTF-8 source" from until)))

(def (line-starts-with? line prefix)
  (and (<= (string-length prefix) (string-length line))
       (string=? (substring line 0 (string-length prefix)) prefix)))

(def (fold-source-lines source initial visit)
  (let (size (string-length source))
    (let loop ((start 0) (cursor 0) (byte-start 0) (state initial))
      (cond
       ((= cursor size)
        (if (= start size) state
          (let* ((line (substring source start size))
                 (byte-end (+ byte-start (string-utf8-length line))))
            (visit line byte-start byte-end state))))
       ((or (char=? (string-ref source cursor) #\newline)
            (char=? (string-ref source cursor) #\return))
        (let* ((after (+ cursor
                        (if (and (char=? (string-ref source cursor) #\return)
                                 (< (+ cursor 1) size)
                                 (char=? (string-ref source (+ cursor 1)) #\newline))
                          2 1)))
               (line (substring source start after))
               (byte-end (+ byte-start (string-utf8-length line))))
          (loop after after byte-end (visit line byte-start byte-end state))))
       (else (loop start (+ cursor 1) byte-start state))))))

(def (source-line-events source root visit)
  (reverse
   (cons '(finish)
         (fold-source-lines source (list (list 'start root))
           (lambda (line start end reversed)
             (foldl cons reversed (visit line start end)))))))
