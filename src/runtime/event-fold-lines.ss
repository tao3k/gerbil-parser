;;; -*- Gerbil -*-
;;; Native byte-view primitives shared by generated code and conformance.
(import (only-in ../compiler/event-fold-future ascii-lower-byte)
        (only-in ./byte-spans byte-span-skip byte-span-all?))
(export current-fold-line-view fold-line-bytes fold-key-byte? fold-ascii-prefix?
        line-starts-with-ascii-ci? fold-line-prefix-boundary? fold-line-blank?
        fold-line-has-key-after-prefix? fold-line-has-word-after-prefix?
        fold-line-name-set-contains? fold-byte-prefix? fold-byte-marker-level)

(def (byte-slice-name-compare bytes from until name)
  (let ((size (string-length name)))
    (let loop ((cursor from) (index 0))
      (cond
       ((= cursor until) (if (= index size) 0 -1))
       ((= index size) 1)
       (else
        (let (difference (- (u8vector-ref bytes cursor)
                            (char->integer (string-ref name index))))
          (if (= difference 0) (loop (+ cursor 1) (+ index 1)) difference)))))))

(def (fold-line-name-set-contains? bytes from until names)
  ;; Names are admitted sorted unique ASCII constants. Compare source bytes
  ;; directly: no slice strings, dense trie, request cache, or unrolled arms.
  (unless (<= 0 from until (u8vector-length bytes))
    (error "native fold name range outside source line"))
  (let search ((lower 0) (upper (vector-length names)))
    (and (< lower upper)
         (let* ((middle (quotient (+ lower upper) 2))
                (comparison (byte-slice-name-compare bytes from until (vector-ref names middle))))
           (cond ((zero? comparison) #t)
                 ((negative? comparison) (search lower middle))
                 (else (search (+ middle 1) upper)))))))

;; The immutable UTF-8 view belongs to the active statement frame, not a
;; process-wide memo. Nested helpers bind their own view and restore callers.
(def current-fold-line-view (make-parameter #f))

(def (fold-line-bytes line)
  (let (view (current-fold-line-view))
    (if (and view (eq? line (car view)))
      (cdr view)
      (string->utf8 line))))


(def (line-starts-with-ascii-ci? line prefix
                                  (actual (fold-line-bytes line))
                                  (expected (string->utf8 prefix))
                                  (from 0) (until (u8vector-length actual)))
  (fold-byte-prefix? actual from until expected #t))

(def (fold-byte-prefix? bytes from until expected ascii-ci?)
  (let (size (u8vector-length expected))
    (and (>= (- until from) size)
         (let loop ((index 0))
           (or (= index size)
               (and (let ((actual (u8vector-ref bytes (+ from index)))
                          (wanted (u8vector-ref expected index)))
                      (= (if ascii-ci? (ascii-lower-byte actual) actual)
                         (if ascii-ci? (ascii-lower-byte wanted) wanted)))
                    (loop (+ index 1))))))))

(def (fold-byte-marker-level bytes from until marker boundary)
  (let loop ((cursor from))
    (if (and (< cursor until) (= (u8vector-ref bytes cursor) marker))
      (loop (+ cursor 1))
      (if (and (> cursor from) (< cursor until)
               (= (u8vector-ref bytes cursor) boundary))
        (- cursor from) 0))))

(def (fold-line-blank? line (bytes (fold-line-bytes line))
                      (from 0) (until (u8vector-length bytes)))
  (byte-span-all? bytes from until fold-space-byte?))

(def (fold-space-byte? byte)
  (memv byte '(9 10 13 32)))

(def (fold-horizontal-byte? byte)
  (memv byte '(9 32)))

(def (fold-line-prefix-boundary? line prefix marker?
                                 (bytes (fold-line-bytes line))
                                 (expected (string->utf8 prefix))
                                 (from 0) (end (u8vector-length bytes)))
  (and (line-starts-with-ascii-ci? line prefix bytes expected from end)
       (let ((start (+ from (u8vector-length expected))))
         (if marker?
           (byte-span-all? bytes start end fold-space-byte?)
           (or (= start end)
               (memv (u8vector-ref bytes start) '(9 10 13 32)))))))


(def (fold-ascii-prefix? value)
  (and (string? value)
       (every (lambda (character) (< (char->integer character) 128))
              (string->list value))))

(def (fold-key-byte? byte)
  (or (and (<= 48 byte) (<= byte 57))
      (and (<= 65 byte) (<= byte 90))
      (and (<= 97 byte) (<= byte 122))
      (memv byte '(45 95))))

(def (fold-line-has-key-after-prefix? line prefix
                                      (bytes (fold-line-bytes line))
                                      (expected (string->utf8 prefix))
                                      (from 0) (end (u8vector-length bytes)))
  (and (line-starts-with-ascii-ci? line prefix bytes expected from end)
       (let ((begin (+ from (u8vector-length expected))))
         (let (cursor (byte-span-skip bytes begin end fold-key-byte?))
           (and (> cursor begin) (< cursor end)
                (= (u8vector-ref bytes cursor) 58))))))

(def (fold-line-has-word-after-prefix? line prefix
                                       (bytes (fold-line-bytes line))
                                       (expected (string->utf8 prefix))
                                       (from 0) (end (u8vector-length bytes)))
  (and (line-starts-with-ascii-ci? line prefix bytes expected from end)
       (let (cursor (byte-span-skip bytes (+ from (u8vector-length expected)) end
                                   fold-horizontal-byte?))
         (and (< cursor end) (not (fold-space-byte? (u8vector-ref bytes cursor)))))))
