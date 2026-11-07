;;; -*- Gerbil -*-
;;; Scan basic owner: original pure lexical algorithms.
(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ../token make-token))
(export scan-while identifier-start? identifier-rest? scan-nonempty-while scan-whitespace horizontal-whitespace? scan-horizontal-whitespace newline? scan-newline scan-line scan-character-run scan-until-delimiters scan-identifier line-end skip-horizontal literal-at? scan-emit)

;; Scanner functions return the exclusive source-character end offset or #f.
;; : (-> String Nat (-> Char Boolean) (Maybe Nat))
(def (scan-while source start predicate)
  (let (length (string-length source))
    (let loop ((offset start))
      (if (and (< offset length) (predicate (string-ref source offset)))
        (loop (+ offset 1))
        offset))))

;; identifier-start?
;; : (-> Char Boolean)
(def (identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

;; identifier-rest?
;; : (-> Char Boolean)
(def (identifier-rest? ch)
  (or (identifier-start? ch) (char-numeric? ch) (char=? ch #\-)))

;; scan-nonempty-while
;; : (-> Procedure Procedure String Fixnum Fixnum)
(def (scan-nonempty-while first-predicate rest-predicate source start)
  (and (first-predicate (string-ref source start))
       (scan-while source start rest-predicate)))

;; scan-whitespace
;; : (-> String Fixnum Fixnum)
(def scan-whitespace
  (cut scan-nonempty-while char-whitespace? char-whitespace? <> <>))

;; horizontal-whitespace?
;; : (-> Char Boolean)
(def (horizontal-whitespace? ch)
  (or (char=? ch #\space) (char=? ch #\tab)))

;; scan-horizontal-whitespace
;; : (-> String Fixnum Fixnum)
(def scan-horizontal-whitespace
  (cut scan-nonempty-while
       horizontal-whitespace? horizontal-whitespace? <> <>))

;; newline?
;; : (-> Char Boolean)
(def (newline? ch)
  (or (char=? ch #\newline) (char=? ch #\return)))

;; scan-newline
;; : (-> String Fixnum Fixnum)
(def scan-newline (cut scan-nonempty-while newline? newline? <> <>))

;; One complete line, including its LF, CRLF, or bare CR terminator.
;; This primitive is useful for line-oriented grammars without allocating a
;; substring or emitting one token per source character.
(def (scan-line source start)
  (let (length (string-length source))
    (and (< start length)
         (let loop ((offset start))
           (cond
            ((= offset length) length)
            ((char=? (string-ref source offset) #\newline)
             (+ offset 1))
            ((char=? (string-ref source offset) #\return)
             (if (and (< (+ offset 1) length)
                      (char=? (string-ref source (+ offset 1)) #\newline))
               (+ offset 2)
               (+ offset 1)))
            (else (loop (+ offset 1))))))))

;; A maximal run of one character, admitted only when it meets the declared
;; minimum.  The grammar owns the character and the bound.
(def (scan-character-run source start character minimum)
  (and (string? character)
       (= (string-length character) 1)
       (exact-integer? minimum)
       (positive? minimum)
       (<= 0 start)
       (< start (string-length source))
       (let (end (scan-while
                 source start
                 (lambda (ch) (char=? ch (string-ref character 0)))))
         (and (>= (- end start) minimum) end))))

;; Maximal nonempty atom run, stopping before Unicode whitespace or a delimiter.
;; Delimiters are language-owned; this primitive knows no S-expression policy.
(def (scan-until-delimiters source start delimiters)
  (and (< start (string-length source))
       (let (end (scan-while
                 source start
                 (lambda (character)
                   (and (not (char-whitespace? character))
                        (not (let loop ((index 0))
                               (and (< index (string-length delimiters))
                                    (or (char=? character
                                                (string-ref delimiters index))
                                        (loop (+ index 1))))))))))
         (and (> end start) end))))

;; scan-identifier
;; : (-> String Fixnum Fixnum)
(def scan-identifier
  (cut scan-nonempty-while identifier-start? identifier-rest? <> <>))

(def (line-end source start)
  (scan-while source start
              (lambda (ch)
                (and (not (char=? ch #\newline))
                     (not (char=? ch #\return))))))

(def (skip-horizontal source start end)
  (let loop ((offset start))
    (if (and (< offset end)
             (horizontal-whitespace? (string-ref source offset)))
      (loop (+ offset 1))
      offset)))

(def (literal-at? source start literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ start literal-length) source-length)
         (let loop ((index 0))
           (or (= index literal-length)
               (and (char=? (string-ref source (+ start index))
                            (string-ref literal index))
                    (loop (+ index 1))))))))

(def (scan-emit source kind start end byte-start)
  (let* ((lexeme (substring source start end))
         (byte-end (+ byte-start (string-utf8-length lexeme))))
    (make-token kind lexeme byte-start byte-end)))

