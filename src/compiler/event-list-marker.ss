;;; -*- Gerbil -*-
;;; Generic source-backed list-marker lexing for a bounded Scheme event fold.
(import (only-in ../runtime/byte-spans byte-span-skip))

(export event-line-indent-column scan-event-list-marker
        event-byte-indent-column scan-event-list-marker-bytes)

(def (horizontal? byte)
  (memv byte '(9 32)))

(def (content-end bytes (floor 0) (until (u8vector-length bytes)))
  (let loop ((cursor until))
    (if (and (> cursor floor) (memv (u8vector-ref bytes (- cursor 1)) '(10 13)))
      (loop (- cursor 1)) cursor)))

(def (line-indent bytes limit tab-width (from 0))
  (unless (and (exact-integer? tab-width) (> tab-width 0))
    (error "event list tab width must be positive" tab-width))
  (let loop ((cursor from) (column 0))
    (if (and (< cursor limit) (horizontal? (u8vector-ref bytes cursor)))
      (loop (+ cursor 1)
            (if (= (u8vector-ref bytes cursor) 9)
              (* (+ (quotient column tab-width) 1) tab-width)
              (+ column 1)))
      (values cursor column))))

(def (event-line-indent-column line tab-width)
  (let (bytes (string->utf8 line))
    (event-byte-indent-column bytes 0 (u8vector-length bytes) tab-width)))

(def (event-byte-indent-column bytes from until tab-width)
  (let-values (((_ column) (line-indent bytes (content-end bytes from until) tab-width from)))
    column))

(def (ascii-digit? byte)
  (<= 48 byte 57))

(def (ascii-alpha? byte)
  (or (<= 65 byte 90) (<= 97 byte 122)))

(def (ordered-end bytes first limit)
  (let (byte (u8vector-ref bytes first))
    (cond
     ((ascii-digit? byte)
      (let (cursor (byte-span-skip bytes (+ first 1) limit ascii-digit?))
        (and (< cursor limit) (memv (u8vector-ref bytes cursor) '(41 46))
             (+ cursor 1))))
     ((ascii-alpha? byte)
      (and (< (+ first 1) limit)
           (memv (u8vector-ref bytes (+ first 1)) '(41 46))
           (+ first 2)))
     (else #f))))

(def (scan-event-list-marker line start unordered ordered? tab-width)
  (unless (and (string? unordered) (> (string-length unordered) 0)
               (every (lambda (character) (< (char->integer character) 128))
                      (string->list unordered)))
    (error "event list markers must be nonempty ASCII" unordered))
  (let* ((bytes (string->utf8 line))
         (marker (scan-event-list-marker-bytes bytes 0 (u8vector-length bytes)
                                              (string->utf8 unordered) ordered? tab-width)))
    (when marker
      (for-each (lambda (index) (vector-set! marker index (+ start (vector-ref marker index)))) '(2 3 4)))
    marker))

;; The compiler admits the ASCII marker constant. Byte positions are absolute
;; within the borrowed request buffer; indentation columns stay frame-relative.
(def (scan-event-list-marker-bytes bytes from until unordered-bytes ordered? tab-width)
  (let (limit (content-end bytes from until))
    (let-values (((first column) (line-indent bytes limit tab-width from)))
      (and (< first limit)
           (let* ((byte (u8vector-ref bytes first))
                  (unordered? (let loop ((index 0))
                                (and (< index (u8vector-length unordered-bytes))
                                     (or (= byte (u8vector-ref unordered-bytes index))
                                         (loop (+ index 1))))))
                  (bullet-end
                   (if unordered? (+ first 1)
                     (and ordered? (ordered-end bytes first limit)))))
             (and bullet-end
                  (or (= bullet-end limit)
                      (horizontal? (u8vector-ref bytes bullet-end)))
                  (vector column (not unordered?) first bullet-end
                          (byte-span-skip bytes bullet-end limit horizontal?))))))))
