;;; -*- Gerbil -*-
;;; Scan quoted owner: original pure lexical algorithms.
(import (only-in "./basic.ss" literal-at?))
(export scan-quoted-string/mode scan-quoted-string scan-quoted-strings scan-escaped-quoted-strings scan-balanced-word scan-quoted-string/profile)

;; scan-quoted-string
;;   : (-> String Fixnum String Fixnum)
;;   | doc m%
;;       `scan-quoted-string` scans one escaped, delimiter-bound string.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-quoted-string "'value'" 0 "'")
;;       ;; => 7
;;       ```
;;     %
(def (scan-quoted-string/mode source start delimiter doubled-delimiter?)
  (let ((length (string-length source))
        (delimiter-length (string-length delimiter)))
    (and (literal-at? source start delimiter)
         (let loop ((offset (+ start delimiter-length)) (escaped? #f))
           (cond
            ((>= offset length) #f)
            (escaped? (loop (+ offset 1) #f))
            ((char=? (string-ref source offset) #\\)
             (loop (+ offset 1) #t))
            ((literal-at? source offset delimiter)
             (let (next (+ offset delimiter-length))
               ;; ISO graph-query character sequences escape their delimiter
               ;; by doubling it.  Backslash escaping remains admitted for
               ;; the same source grammars, so both forms stay lossless.
               (if (and doubled-delimiter?
                        (literal-at? source next delimiter))
                 (loop (+ next delimiter-length) #f)
                 next)))
            (else (loop (+ offset 1) #f)))))))

(def (scan-quoted-string source start delimiter)
  (scan-quoted-string/mode source start delimiter #t))

;; scan-quoted-strings
;;   : (-> String Fixnum List Fixnum)
;;   | doc m%
;;       `scan-quoted-strings` tries delimiters in declaration order.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-quoted-strings "\"value\"" 0 '("\"" "'"))
;;       ;; => 7
;;       ```
;;     %
(def (scan-quoted-strings source start delimiters)
  (ormap (lambda (delimiter)
           (scan-quoted-string source start delimiter))
         delimiters))

;; Backslash escapes are admitted; adjacent quoted strings remain distinct.
(def (scan-escaped-quoted-strings source start delimiters)
  (ormap (lambda (delimiter)
           (scan-quoted-string/mode source start delimiter #f))
         delimiters))

;;; A closed scanner primitive for words containing quoted or balanced spans.
;;; The language supplies only delimiter data. The scanner owns recursion,
;;; source bounds, and failure on an unterminated span.
(def (scan-balanced-word source start stops quotes pairs)
  (let (length (string-length source))
    (def (at? offset text)
      (let (end (+ offset (string-length text)))
        (and (<= end length)
             (string=? (substring source offset end) text))))
    (def (pair-at offset)
      (foldl
       (lambda (entry current)
         (if (and (at? offset (car entry))
                  (or (not current)
                      (> (string-length (car entry))
                         (string-length (car current)))))
           entry current))
       #f pairs))
    (def (quote-at offset)
      (find (lambda (text) (at? offset text)) quotes))
    (def (stopped? offset)
      (or (char-whitespace? (string-ref source offset))
          (ormap (lambda (text) (at? offset text)) stops)))
    (def (after-escape offset)
      (min length (+ offset 2)))
    (def (quoted-end offset quote)
      (let (delimiter (string-ref quote 0))
        (let loop ((cursor (+ offset 1)))
          (cond
           ((= cursor length)
            (error "unterminated balanced word quote" offset))
           ((char=? (string-ref source cursor) delimiter)
            (+ cursor 1))
           ((and (not (char=? delimiter #\'))
                 (char=? (string-ref source cursor) #\\))
            (loop (after-escape cursor)))
           ((and (char=? delimiter #\") (pair-at cursor))
            (loop (paired-end cursor (pair-at cursor))))
           (else (loop (+ cursor 1)))))))
    (def (paired-end offset entry)
      (let ((opening (cadr entry))
            (closing (caddr entry))
            (prefix (car entry)))
        (let loop ((cursor (+ offset (string-length prefix))) (depth 1))
          (cond
           ((= cursor length)
            (error "unterminated balanced word pair" offset prefix))
           ((char=? (string-ref source cursor) #\\)
            (loop (after-escape cursor) depth))
           ((quote-at cursor)
            (loop (quoted-end cursor (quote-at cursor)) depth))
           ((pair-at cursor)
            (loop (paired-end cursor (pair-at cursor)) depth))
           ((char=? (string-ref source cursor) opening)
            (loop (+ cursor 1) (+ depth 1)))
           ((char=? (string-ref source cursor) closing)
            (if (= depth 1) (+ cursor 1)
              (loop (+ cursor 1) (- depth 1))))
           (else (loop (+ cursor 1) depth))))))
    (and (< start length)
         (not (stopped? start))
         (let loop ((cursor start))
           (cond
            ((= cursor length) cursor)
            ((and (> cursor start) (stopped? cursor)) cursor)
            ((char=? (string-ref source cursor) #\\)
             (loop (after-escape cursor)))
            ((quote-at cursor)
             (loop (quoted-end cursor (quote-at cursor))))
            ((pair-at cursor)
             (loop (paired-end cursor (pair-at cursor))))
            (else (loop (+ cursor 1))))))))

;;; A closed lexical profile validates escaped source without decoding it.
;;; Simple escapes consume one character; \u consumes exactly the configured
;;; number of hex characters. Zero disables that escape; adjacent quotes remain
;;; distinct. Numeric hex characters retain the existing Scheme lexical class.
(def (scan-quoted-string/profile source start delimiter escapes unicode-width)
  (let ((length (string-length source)) (delimiter-char (string-ref delimiter 0)))
    (def (hex? ch)
      (or (char-numeric? ch)
          (and (char>=? ch #\a) (char<=? ch #\f))
          (and (char>=? ch #\A) (char<=? ch #\F))))
    (def (simple? ch)
      (let loop ((index 0))
        (and (< index (string-length escapes))
             (or (char=? ch (string-ref escapes index)) (loop (+ index 1))))))
    (def (escape-end slash)
      (let (next (+ slash 1))
        (and (< next length)
             (let (ch (string-ref source next))
               (cond
                ((simple? ch) (+ next 1))
                ((and (positive? unicode-width) (char=? ch #\u))
                 (let (end (+ next 1 unicode-width))
                   (and (<= end length)
                        (let check ((index (+ next 1)))
                          (or (= index end)
                              (and (hex? (string-ref source index))
                                   (check (+ index 1)))))
                        end)))
                (else #f))))))
    (and (< start length) (char=? (string-ref source start) delimiter-char)
         (let loop ((offset (+ start 1)))
           (and (< offset length)
                (let (ch (string-ref source offset))
                  (cond
                   ((char=? ch delimiter-char) (+ offset 1))
                   ((char=? ch #\\) (alet (end (escape-end offset)) (loop end)))
                   (else (loop (+ offset 1))))))))))

