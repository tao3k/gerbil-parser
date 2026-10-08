;;; -*- Gerbil -*-
;;; Pure projections of source patterns into the shared event-fold algebra.
;;; Strings are literal grammar inputs, not a second runtime matcher or DSL.

(import (only-in :std/string/utf8 string-utf8-length))
(export source-offset-after source-pattern-end source-pattern-at?
        source-ascii-ci-pattern-at?)

(def (source-offset-after from count)
  (unless (and (exact-integer? count) (>= count 0))
    (error "source byte offset requires a nonnegative exact step count" count))
  (let loop ((offset from) (remaining count))
    (if (= remaining 0) offset
      (loop `(line-step ,offset) (- remaining 1)))))

(def (source-pattern-end from pattern)
  (unless (string? pattern)
    (error "source pattern must be a string" pattern))
  (source-offset-after from (string-utf8-length pattern)))

(def (source-pattern-at? from pattern)
  (unless (and (string? pattern) (> (string-length pattern) 0))
    (error "source byte pattern must not be empty" pattern))
  (let (bytes (string->utf8 pattern))
    (let loop ((offset from) (index 0) (predicate #f))
      (if (= index (u8vector-length bytes)) predicate
        (let (check `(line-byte-equal? ,offset ,(u8vector-ref bytes index)))
          (loop `(line-step ,offset) (+ index 1)
                (if predicate `(and ,predicate ,check) check)))))))

(def (source-ascii-ci-pattern-at? from pattern)
  (unless (and (string? pattern) (> (string-length pattern) 0)
               (every (lambda (char) (< (char->integer char) 128))
                      (string->list pattern)))
    (error "case-insensitive source pattern requires nonempty ASCII" pattern))
  (let loop ((rest (string->list pattern)) (offset from) (predicates []))
    (if (null? rest)
      ;; A single predicate must not become an invalid unary `and` form.
      (if (null? (cdr predicates)) (car predicates)
          (cons 'and (reverse predicates)))
      (let* ((byte (char->integer (car rest)))
             (upper (if (<= 97 byte 122) (- byte 32) byte))
             (lower (if (<= 65 byte 90) (+ byte 32) byte))
             (check (if (= upper lower)
                      `(line-byte-equal? ,offset ,upper)
                      `(line-bytes-any-in? ,offset (line-step ,offset)
                                           (,upper ,lower)))))
        (loop (cdr rest) `(line-step ,offset) (cons check predicates))))))
