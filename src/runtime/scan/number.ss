;;; -*- Gerbil -*-
;;; Scan number owner: original pure lexical algorithms.
(import (only-in "./basic.ss" scan-while scan-nonempty-while)
        (only-in "./literal.ss" scan-longest-literal))
(export scan-decimal-digits scan-number-fraction-end scan-number-exponent-digits-position scan-number-exponent-end scan-number-literal ascii-digit-value ascii-digit-for-base? scan-separated-digits numeric-prefix-base scan-radix-number scan-profile-decimal-mantissa scan-profile-exponent scan-number-literal/profile)

;; scan-decimal-digits
;; : (-> String Fixnum Fixnum)
(def scan-decimal-digits
  (cut scan-nonempty-while char-numeric? char-numeric? <> <>))

;; scan-number-fraction-end
;; : (-> String Fixnum Fixnum Fixnum)
(def (scan-number-fraction-end source whole-end length)
  (if (and (< whole-end length)
           (char=? (string-ref source whole-end) #\.)
           (< (+ whole-end 1) length)
           (char-numeric? (string-ref source (+ whole-end 1))))
    (scan-while source (+ whole-end 1) char-numeric?)
    whole-end))

;; scan-number-exponent-digits-position
;; : (-> String Fixnum Fixnum Fixnum)
(def (scan-number-exponent-digits-position source exponent-end length)
  (let (position (+ exponent-end 1))
    (if (and (< position length)
             (memv (string-ref source position) '(#\+ #\-)))
      (+ position 1)
      position)))

;; scan-number-exponent-end
;; : (-> String Fixnum Fixnum Fixnum)
(def (scan-number-exponent-end source fraction-end length)
  (if (and (< fraction-end length)
           (memv (string-ref source fraction-end) '(#\e #\E)))
    (let (digits-position
          (scan-number-exponent-digits-position source fraction-end length))
      (if (and (< digits-position length)
               (char-numeric? (string-ref source digits-position)))
        (scan-while source digits-position char-numeric?)
        fraction-end))
    fraction-end))

(def (scan-number-literal source start)
  (and (char-numeric? (string-ref source start))
       (let* ((length (string-length source))
              (whole-end (scan-while source start char-numeric?))
              (fraction-end
               (scan-number-fraction-end source whole-end length)))
         (scan-number-exponent-end source fraction-end length))))

;; : (-> Char (Maybe Fixnum))
(def (ascii-digit-value ch)
  (cond
   ((and (char>=? ch #\0) (char<=? ch #\9))
    (- (char->integer ch) (char->integer #\0)))
   ((and (char>=? ch #\a) (char<=? ch #\f))
    (+ 10 (- (char->integer ch) (char->integer #\a))))
   ((and (char>=? ch #\A) (char<=? ch #\F))
    (+ 10 (- (char->integer ch) (char->integer #\A))))
   (else #f)))

;; : (-> Char Fixnum Boolean)
(def (ascii-digit-for-base? ch base)
  (alet (value (ascii-digit-value ch))
    (< value base)))

;; : (-> String Fixnum Fixnum Char (Maybe Fixnum))
(def (scan-separated-digits source start base separator)
  (let (length (string-length source))
    (and (< start length)
         (ascii-digit-for-base? (string-ref source start) base)
         (let loop ((offset (+ start 1)))
           (cond
            ((>= offset length) offset)
            ((ascii-digit-for-base? (string-ref source offset) base)
             (loop (+ offset 1)))
            ((and (char=? (string-ref source offset) separator)
                  (< (+ offset 1) length)
                  (ascii-digit-for-base?
                   (string-ref source (+ offset 1)) base))
             (loop (+ offset 2)))
            (else offset))))))

;; : (-> String Fixnum)
(def (numeric-prefix-base prefix)
  (let (last (char-downcase
              (string-ref prefix (- (string-length prefix) 1))))
    (case last
      ((#\b) 2)
      ((#\o) 8)
      ((#\x) 16)
      (else (error "numeric radix prefix must end in b, o, or x" prefix)))))

;; : (-> String Fixnum [String] Char (Maybe Fixnum))
(def (scan-radix-number source start prefixes separator)
  (alet (prefix (scan-longest-literal source start prefixes))
    (scan-separated-digits
     source (+ start (string-length prefix))
     (numeric-prefix-base prefix) separator)))

;; : (-> String Fixnum Char Boolean Boolean (Maybe Fixnum))
(def (scan-profile-decimal-mantissa
      source start separator leading-period? trailing-period?)
  (let (length (string-length source))
    (cond
     ((and leading-period?
           (< (+ start 1) length)
           (char=? (string-ref source start) #\.)
           (ascii-digit-for-base? (string-ref source (+ start 1)) 10))
      (scan-separated-digits source (+ start 1) 10 separator))
     ((and (< start length)
           (ascii-digit-for-base? (string-ref source start) 10))
      (let* ((whole-end
              (scan-separated-digits source start 10 separator))
             (period? (and (< whole-end length)
                           (char=? (string-ref source whole-end) #\.))))
        (if period?
          (or (scan-separated-digits source (+ whole-end 1) 10 separator)
              (and trailing-period? (+ whole-end 1))
              whole-end)
          whole-end)))
     (else #f))))

;; : (-> String Fixnum Char Fixnum)
(def (scan-profile-exponent source mantissa-end separator)
  (let (length (string-length source))
    (if (and (< mantissa-end length)
             (memv (string-ref source mantissa-end) '(#\e #\E)))
      (let* ((after-indicator (+ mantissa-end 1))
             (digits-start
              (if (and (< after-indicator length)
                       (memv (string-ref source after-indicator) '(#\+ #\-)))
                (+ after-indicator 1)
                after-indicator)))
        (or (scan-separated-digits source digits-start 10 separator)
            mantissa-end))
      mantissa-end)))

;; scan-number-literal/profile
;;   : (-> String Fixnum [String] String [String] Boolean Boolean (Maybe Fixnum))
;;   | doc m%
;;       Scans one grammar-declared numeric profile without assigning a
;;       language identity in runtime code.
;;     %
(def (scan-number-literal/profile
      source start prefixes separator suffixes leading-period? trailing-period?)
  (let (separator-character (string-ref separator 0))
    (or (scan-radix-number source start prefixes separator-character)
        (alet* ((mantissa-end
                 (scan-profile-decimal-mantissa
                  source start separator-character
                  leading-period? trailing-period?))
                (number-end
                 (scan-profile-exponent source mantissa-end separator-character)))
          (or (alet (suffix (scan-longest-literal source number-end suffixes))
                (+ number-end (string-length suffix)))
              number-end)))))

