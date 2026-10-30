;;; Public pure text operations; no compiler or native scanner initialization.
(import (only-in :std/string/misc string-trim)
        (only-in ./src/utilities/strings string-index-from))
(export string-before ascii-ci=? string-after string-first-word string-rest-after-first-word string-last-word string-before-last-word string-trim-start string-replace string-lowercase string-prefix? string-suffix? string-words string-in? string-single-ascii-uppercase? string-unsigned-at-most?)

(def (string-single-ascii-uppercase? value)
  (and (= (string-length value) 1)
       (char<=? #\A (string-ref value 0) #\Z)))

(def (string-unsigned-at-most? value maximum)
  (let (number (string->number value 10))
    (and (> (string-length value) 0)
         (andmap (lambda (char) (char<=? #\0 char #\9))
                 (string->list value))
         (integer? number) (exact? number)
         (<= 0 number maximum))))

(def (string-before value delimiter)
  (let (index (string-index-from value delimiter))
    (if (fixnum? index) (substring value 0 index) value)))

(def (string-after value delimiter)
  (let (index (string-index-from value delimiter))
    (if (fixnum? index)
      (substring value (+ index (string-length delimiter))
                 (string-length value))
      "")))

(def (string-replace value needle replacement)
  (when (equal? needle "")
    (error "pure string replacement needs a nonempty needle"))
  (let loop ((from 0) (parts '()))
    (let (at (string-index-from value needle from))
      (if (fixnum? at)
        (loop (+ at (string-length needle))
              (cons replacement (cons (substring value from at) parts)))
        (apply string-append
               (reverse (cons (substring value from (string-length value))
                              parts)))))))

(def (string-lowercase value)
  (list->string (map char-downcase (string->list value))))

(def (string-trim-start value)
  (let (size (string-length value))
    (let loop ((index 0))
      (if (or (= index size)
              (not (char-whitespace? (string-ref value index))))
        (substring value index size)
        (loop (+ index 1))))))

(def (ascii-fold char)
  (let (code (char->integer char))
    (if (and (<= 65 code) (<= code 90))
      (integer->char (+ code 32))
      char)))

(def (ascii-ci=? left right)
  (and (= (string-length left) (string-length right))
       (let loop ((index 0))
         (or (= index (string-length left))
             (and (char=? (ascii-fold (string-ref left index))
                          (ascii-fold (string-ref right index)))
                  (loop (+ index 1)))))))

(def (string-first-word value)
  (let* ((text (string-trim value)) (size (string-length text)))
    (let loop ((index 0))
      (if (or (= index size)
              (char-whitespace? (string-ref text index)))
        (substring text 0 index)
        (loop (+ index 1))))))

(def (string-rest-after-first-word value)
  (let* ((text (string-trim value)) (size (string-length text)))
    (let loop ((index 0))
      (cond
       ((= index size) "")
       ((char-whitespace? (string-ref text index))
        (string-trim (substring text (+ index 1) size)))
       (else (loop (+ index 1)))))))

(def (string-last-word value)
  (let (words (string-words value))
    (if (null? words) "" (car (reverse words)))))

(def (string-before-last-word value)
  (let* ((text (string-trim value)) (size (string-length text)))
    (let loop ((index (- size 1)))
      (cond
       ((< index 0) "")
       ((char-whitespace? (string-ref text index))
        (string-trim (substring text 0 index)))
       (else (loop (- index 1)))))))

(def (string-prefix? value prefix)
  (and (<= (string-length prefix) (string-length value))
       (equal? prefix (substring value 0 (string-length prefix)))))

(def (string-suffix? value suffix)
  (let ((value-size (string-length value))
        (suffix-size (string-length suffix)))
    (and (<= suffix-size value-size)
         (equal? suffix (substring value (- value-size suffix-size) value-size)))))

(def (string-words value)
  (let (size (string-length value))
    (let loop ((index 0) (start #f) (words '()))
      (if (= index size)
        (reverse (if start (cons (substring value start size) words) words))
        (if (char-whitespace? (string-ref value index))
          (loop (+ index 1) #f
                (if start (cons (substring value start index) words) words))
          (loop (+ index 1) (or start index) words))))))

(def (string-in? value collection)
  (if (member value collection) #t #f))

