;;; -*- Gerbil -*-
;;; Language-neutral scanner primitives used by generated lexers.

(import ./token)
(export scan-whitespace
        scan-decimal-digits
        scan-identifier
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

(def (scan-decimal-digits source start)
  (and (char-numeric? (string-ref source start))
       (scan-while source start char-numeric?)))

(def (scan-identifier source start)
  (and (identifier-start? (string-ref source start))
       (scan-while source start identifier-rest?)))

(def (literal-at? source start literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ start literal-length) source-length)
         (string=? (substring source start (+ start literal-length)) literal))))

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
