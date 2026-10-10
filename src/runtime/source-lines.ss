;;; -*- Gerbil -*-
;;; Request source traversal has no grammar/compiler initialization dependency.
(import (only-in :std/string/utf8 string-utf8-length utf8->string))
(export fold-source-lines fold-source-byte-lines source-line-events line-starts-with?)

;; Generated native products already own the UTF-8 source. Traverse that
;; representation once, retaining each frame's bytes instead of encoding its
;; decoded line again. ASCII CR/LF cannot occur inside a UTF-8 continuation.
(def (fold-source-byte-lines bytes initial visit)
  (let (size (u8vector-length bytes))
    (def (emit from until state)
      (let (line-bytes (subu8vector bytes from until))
        (visit (utf8->string line-bytes) line-bytes from until state)))
    (let loop ((start 0) (cursor 0) (state initial))
      (cond
       ((= cursor size)
        (if (= start size) state (emit start size state)))
       ((memv (u8vector-ref bytes cursor) '(10 13))
        (let (after (+ cursor
                       (if (and (= (u8vector-ref bytes cursor) 13)
                                (< (+ cursor 1) size)
                                (= (u8vector-ref bytes (+ cursor 1)) 10))
                         2 1)))
          (loop after after (emit start after state))))
       (else (loop start (+ cursor 1) state))))))

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
