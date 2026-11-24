;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Pure, language-neutral UTF-8 byte-span structural edit algebra.
;;;
;;; Edits operate strictly on UTF-8 byte intervals [start, end) emitted by a
;;; parser. Overlapping intervals and out-of-bounds offsets fail closed.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/type validate)
        (only-in ./span-edit-types
                 +span-edit-kind+ +span-edit-result-kind+
                 SpanEdit SpanEditResult span-edit?))
(export make-span-edit
        span-edit-insert
        span-edit-delete
        span-edit-replace
        span-edit-start
        span-edit-end
        span-edit-replacement
        span-edit-label
        utf8-byte-slice
        span-edits-normalize
        apply-span-edits
        apply-span-edits/verified)

(def (make-span-edit start-val end-val replacement-val label: (label-val #f))
  (validate SpanEdit
            (.o bind-these: #f
                kind: +span-edit-kind+
                start: start-val
                end: end-val
                replacement: replacement-val
                label: label-val)))

(def (span-edit-insert offset text label: (label #f))
  (make-span-edit offset offset text label: label))

(def (span-edit-delete start end label: (label #f))
  (make-span-edit start end "" label: label))

(def (span-edit-replace start end text label: (label #f))
  (make-span-edit start end text label: label))

(def (span-edit-start edit) (.ref edit 'start))
(def (span-edit-end edit) (.ref edit 'end))
(def (span-edit-replacement edit) (.ref edit 'replacement))
(def (span-edit-label edit) (.ref edit 'label))

(def (utf8-byte-slice bytes start end)
  (unless (u8vector? bytes)
    (error "utf8-byte-slice requires a u8vector" bytes))
  (let ((total (u8vector-length bytes)))
    (unless (and (exact-integer? start) (exact-integer? end)
                 (<= 0 start end total))
      (error "UTF-8 byte slice out of bounds" start end total))
    (utf8->string (subu8vector bytes start end))))

;; Sort edits by ascending (start, end, original-index) and fail closed on any
;; out-of-bounds offset or overlapping byte interval.
(def (span-edits-normalize byte-size edits)
  (unless (and (exact-integer? byte-size) (>= byte-size 0))
    (error "span-edits-normalize requires a non-negative byte size" byte-size))
  (unless (list? edits)
    (error "span-edits-normalize requires a list of SpanEdit values" edits))
  (let* ((indexed
          (let loop ((rest edits) (idx 0) (acc '()))
            (if (null? rest)
              (reverse acc)
              (let ((e (car rest)))
                (unless (span-edit? e)
                  (error "expected validated SpanEdit value" e))
                (let ((s (span-edit-start e))
                      (end (span-edit-end e)))
                  (when (> end byte-size)
                    (error "SpanEdit exceeds UTF-8 source byte length"
                           s end byte-size (span-edit-label e)))
                  (loop (cdr rest) (+ idx 1) (cons (cons idx e) acc)))))))
         (sorted
          (list-sort
                (lambda (a b)
                  (let* ((ea (cdr a)) (eb (cdr b))
                         (sa (span-edit-start ea)) (sb (span-edit-start eb))
                         (enda (span-edit-end ea)) (endb (span-edit-end eb)))
                    (cond
                     ((< sa sb) #t)
                     ((> sa sb) #f)
                     ((< enda endb) #t)
                     ((> enda endb) #f)
                     (else (< (car a) (car b))))))
                indexed)))
    (let check ((rest sorted) (prev-end 0) (prev-label #f))
      (unless (null? rest)
        (let* ((e (cdr (car rest)))
               (s (span-edit-start e))
               (end (span-edit-end e)))
          (when (< s prev-end)
            (error "overlapping SpanEdit intervals rejected"
                   prev-end s prev-label (span-edit-label e)))
          (check (cdr rest) end (span-edit-label e)))))
    (map cdr sorted)))

(def (u8vector-concat chunks)
  (let* ((total (let loop ((cs chunks) (n 0))
                  (if (null? cs) n
                    (loop (cdr cs) (+ n (u8vector-length (car cs)))))))
         (out (make-u8vector total 0)))
    (let loop ((cs chunks) (pos 0))
      (unless (null? cs)
        (let* ((c (car cs))
               (len (u8vector-length c)))
          (when (> len 0)
            (subu8vector-move! c 0 len out pos))
          (loop (cdr cs) (+ pos len)))))
    out))

(def (apply-span-edits source-text edits)
  (unless (string? source-text)
    (error "apply-span-edits requires a source string" source-text))
  (let* ((bytes (string->utf8 source-text))
         (total (u8vector-length bytes))
         (ordered (span-edits-normalize total edits)))
    (if (null? ordered)
      source-text
      (let loop ((rest ordered) (cursor 0) (chunks '()))
        (if (null? rest)
          (let* ((tail (if (< cursor total)
                         (cons (subu8vector bytes cursor total) chunks)
                         chunks))
                 (merged (u8vector-concat (reverse tail))))
            (utf8->string merged))
          (let* ((e (car rest))
                 (s (span-edit-start e))
                 (end (span-edit-end e))
                 (rep-bytes (string->utf8 (span-edit-replacement e)))
                 (chunks1 (if (> s cursor)
                            (cons (subu8vector bytes cursor s) chunks)
                            chunks))
                 (chunks2 (if (> (u8vector-length rep-bytes) 0)
                            (cons rep-bytes chunks1)
                            chunks1)))
            (loop (cdr rest) end chunks2)))))))

(def (apply-span-edits/verified source-text edits verifier)
  (unless (procedure? verifier)
    (error "apply-span-edits/verified requires a parser verifier procedure" verifier))
  (let* ((orig-bytes (u8vector-length (string->utf8 source-text)))
         (next-text (apply-span-edits source-text edits))
         (next-bytes (u8vector-length (string->utf8 next-text)))
         (parsed (verifier next-text)))
    (validate SpanEditResult
              (.o bind-these: #f
                  kind: +span-edit-result-kind+
                  source: next-text
                  applied-count: (length edits)
                  byte-delta: (- next-bytes orig-bytes)
                  verified?: #t
                  parsed: parsed))))
