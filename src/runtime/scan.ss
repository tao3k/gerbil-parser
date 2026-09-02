;;; -*- Gerbil -*-
;;; Language-neutral scanner primitives used by generated lexers.

(import ./token)
(export scan-whitespace
        scan-horizontal-whitespace
        scan-newline
        scan-decimal-digits
        scan-identifier
        scan-quoted-string
        scan-line-comment
        scan-block-comment
        scan-longest-literal
        scan-emit)

(def (scan-while source start predicate)
  (let (length (string-length source))
    (let loop ((offset start))
      (if (and (< offset length) (predicate (string-ref source offset)))
        (loop (+ offset 1))
        offset))))

(def (identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(def (identifier-rest? ch)
  (or (identifier-start? ch) (char-numeric? ch)))

(def (scan-whitespace source start)
  (and (char-whitespace? (string-ref source start))
       (scan-while source start char-whitespace?)))

(def (horizontal-whitespace? ch)
  (or (char=? ch #\space) (char=? ch #\tab)))

(def (scan-horizontal-whitespace source start)
  (and (horizontal-whitespace? (string-ref source start))
       (scan-while source start horizontal-whitespace?)))

(def (newline? ch)
  (or (char=? ch #\newline) (char=? ch #\return)))

(def (scan-newline source start)
  (and (newline? (string-ref source start))
       (scan-while source start newline?)))

(def (scan-decimal-digits source start)
  (and (char-numeric? (string-ref source start))
       (scan-while source start char-numeric?)))

(def (scan-identifier source start)
  (and (identifier-start? (string-ref source start))
       (scan-while source start identifier-rest?)))

(def (scan-quoted-string source start delimiter)
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
             (+ offset delimiter-length))
            (else (loop (+ offset 1) #f)))))))

(def (scan-line-comment source start prefixes)
  (let (prefix (scan-longest-literal source start prefixes))
    (and prefix
         (scan-while source (+ start (string-length prefix))
                     (lambda (ch) (not (newline? ch)))))))

(def (scan-block-comment source start opening closing)
  (let ((length (string-length source))
        (closing-length (string-length closing)))
    (and (literal-at? source start opening)
         (let loop ((offset (+ start (string-length opening))))
           (cond
            ((>= offset length) #f)
            ((literal-at? source offset closing)
             (+ offset closing-length))
            (else (loop (+ offset 1))))))))

(def (literal-at? source start literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ start literal-length) source-length)
         (let loop ((index 0))
           (or (= index literal-length)
               (and (char=? (string-ref source (+ start index))
                            (string-ref literal index))
                    (loop (+ index 1))))))))

(def (scan-longest-literal source start literals)
  (let loop ((rest literals) (selected #f))
    (if (null? rest)
      selected
      (let (candidate (car rest))
        (loop
         (cdr rest)
         (if (and (literal-at? source start candidate)
                  (or (not selected)
                      (> (string-length candidate)
                         (string-length selected))))
           candidate
           selected))))))

(def (scan-emit source kind start end byte-start)
  (let* ((lexeme (substring source start end))
         (byte-end (+ byte-start (u8vector-length (string->utf8 lexeme)))))
    (make-token kind lexeme byte-start byte-end)))
