;;; -*- Gerbil -*-
;;; Language-neutral scanner primitives used by generated lexers.

(import (only-in :std/func any-of)
        (only-in :std/vector/vector vector-map/index)
        (only-in ./token make-token))

(export scan-whitespace
        scan-horizontal-whitespace
        scan-newline
        scan-line
        scan-until-delimiters
        scan-decimal-digits
        scan-number-literal
        scan-number-literal/profile
        scan-identifier
        scan-quoted-string
        scan-quoted-strings
        scan-escaped-quoted-strings
        scan-heredoc
        scan-line-comment
        scan-block-comment
        scan-nested-block-comment
        make-literal-end-scanner
        make-ranked-literal-scanner
        make-ranked-regular-scanner
        scan-longest-literal
        scan-emit)

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
  (any-of (list char-alphabetic? (cut char=? <> #\_)) ch))

;; identifier-rest?
;; : (-> Char Boolean)
(def (identifier-rest? ch)
  (any-of (list identifier-start? char-numeric? (cut char=? <> #\-)) ch))

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
  (any-of (list (cut char=? <> #\space) (cut char=? <> #\tab)) ch))

;; scan-horizontal-whitespace
;; : (-> String Fixnum Fixnum)
(def scan-horizontal-whitespace
  (cut scan-nonempty-while
       horizontal-whitespace? horizontal-whitespace? <> <>))

;; newline?
;; : (-> Char Boolean)
(def (newline? ch)
  (any-of (list (cut char=? <> #\newline) (cut char=? <> #\return)) ch))

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

;; scan-identifier
;; : (-> String Fixnum Fixnum)
(def scan-identifier
  (cut scan-nonempty-while identifier-start? identifier-rest? <> <>))

;;; Closed run expressions share one subset DFA. The number rule has pending
;;; fraction/exponent states; the last accepting state is the maximal match.
;;; Mode-specific winners are compiled once.
(def (regular-kind-bit kind)
  (case kind
    ((whitespace+) 1)
    ((horizontal-whitespace+) 2)
    ((newline+) 4)
    ((decimal-digit+) 8)
    ((identifier) 16)
    ((number) 32)
    (else (error "unsupported regular lexical expression" kind))))

(def (regular-character-mask ch first?)
  (bitwise-ior
   (if (char-whitespace? ch) 1 0)
   (if (horizontal-whitespace? ch) 2 0)
   (if (newline? ch) 4 0)
   (if (char-numeric? ch) 8 0)
   (if (if first? (identifier-start? ch) (identifier-rest? ch)) 16 0)
   (if (and first? (char-numeric? ch)) 32 0)))

(def regular-ascii-first
  (vector-map/index
   (lambda (index _) (regular-character-mask (integer->char index) #t))
   (make-vector 128)))
(def regular-ascii-rest
  (vector-map/index
   (lambda (index _) (regular-character-mask (integer->char index) #f))
   (make-vector 128)))

(def (regular-mask ch first?)
  (let (code (char->integer ch))
    (if (< code 128)
      (vector-ref (if first? regular-ascii-first regular-ascii-rest) code)
      (regular-character-mask ch first?))))

;; Number states: whole=32, dot=64, fraction=128, exponent=256,
;; sign=512, exponent-digits=1024. Only whole/fraction/exponent-digits accept.
(def (regular-accept-mask active)
  (bitwise-ior
   (bitwise-and active 31)
   (if (zero? (bitwise-and active 1184)) 0 32)))

(def (regular-number-transition active ch numeric?)
  (cond
   (numeric?
    (bitwise-ior
     (if (zero? (bitwise-and active 32)) 0 32)
     (if (zero? (bitwise-and active 192)) 0 128)
     (if (zero? (bitwise-and active 1792)) 0 1024)))
   ((char=? ch #\.)
    (if (zero? (bitwise-and active 32)) 0 64))
   ((or (char=? ch #\e) (char=? ch #\E))
    (if (zero? (bitwise-and active 160)) 0 256))
   ((or (char=? ch #\+) (char=? ch #\-))
    (if (zero? (bitwise-and active 256)) 0 512))
   (else 0)))

(def (regular-transition active ch)
  (let (mask (regular-mask ch #f))
    (bitwise-ior
     (bitwise-and active mask)
     (regular-number-transition
      active ch (not (zero? (bitwise-and mask 8)))))))

;;; entries: (expression-kind token-name precedence declaration-index).
;;; A lexical mode admits whole rules before building this scanner.
(def (make-ranked-regular-scanner entries)
  (let* ((available
          (fold (lambda (entry mask)
                  (bitwise-ior mask (regular-kind-bit (car entry))))
                0 entries))
         (winners
          (vector-map/index
           (lambda (mask _)
             (fold
              (lambda (entry best)
                (if (zero? (bitwise-and mask (regular-kind-bit (car entry))))
                  best
                  (let (candidate (cdr entry))
                    (if (or (not best)
                            (> (cadr candidate) (cadr best))
                            (and (= (cadr candidate) (cadr best))
                                 (< (caddr candidate) (caddr best))))
                      candidate best))))
              #f entries))
           (make-vector (if (zero? (bitwise-and available 32)) 32 64)))))
    (if (zero? (bitwise-and available 32))
      (lambda (source start)
        (let* ((length (string-length source))
               (initial
                (and (< start length)
                     (bitwise-and available
                                  (regular-mask (string-ref source start) #t)))))
          (and initial
               (not (zero? initial))
               (let loop ((offset (+ start 1)) (active initial))
                 (let (next
                       (if (< offset length)
                         (bitwise-and active
                                      (regular-mask (string-ref source offset) #f))
                         0))
                   (if (zero? next)
                     (let (winner (vector-ref winners active))
                       (list (car winner) offset
                             (cadr winner) (caddr winner)))
                     (loop (+ offset 1) next)))))))
      (lambda (source start)
        (let* ((length (string-length source))
               (initial
                (and (< start length)
                     (bitwise-and available
                                  (regular-mask (string-ref source start) #t)))))
          (and initial
               (not (zero? initial))
               (let loop ((offset (+ start 1)) (active initial)
                          (accepted initial) (accepted-end (+ start 1)))
                 (let* ((next
                         (if (< offset length)
                           (regular-transition active (string-ref source offset))
                           0))
                        (accept (regular-accept-mask next)))
                   (cond
                    ((zero? next)
                     (let (winner (vector-ref winners accepted))
                       (list (car winner) accepted-end
                             (cadr winner) (caddr winner))))
                    ((zero? accept)
                     (loop (+ offset 1) next accepted accepted-end))
                    (else
                     (loop (+ offset 1) next accept (+ offset 1))))))))))))

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

;; scan-heredoc
;;   : (-> String Fixnum Fixnum)
;;   | doc m%
;;       `scan-heredoc` scans a marker-delimited multiline literal.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-heredoc "<<EOF\nvalue\nEOF" 0)
;;       ;; => 15
;;       ```
;;     %
(def (scan-heredoc source start)
  (let (length (string-length source))
    (and (literal-at? source start "<<")
         (let* ((marker-start0 (+ start 2))
                (marker-start
                 (if (and (< marker-start0 length)
                          (char=? (string-ref source marker-start0) #\-))
                   (+ marker-start0 1)
                   marker-start0))
                (marker-end
                 (and (< marker-start length)
                      (identifier-start? (string-ref source marker-start))
                      (scan-while source marker-start identifier-rest?))))
           (and marker-end
                (< marker-end length)
                (newline? (string-ref source marker-end))
                (let ((marker (substring source marker-start marker-end))
                      (body-start (+ marker-end 1)))
                  (let loop ((line-start body-start))
                    (and (< line-start length)
                         (let* ((end (line-end source line-start))
                                (content-start
                                 (skip-horizontal source line-start end)))
                           (if (and (literal-at? source content-start marker)
                                    (= (+ content-start
                                          (string-length marker))
                                       end))
                             end
                             (and (< end length)
                                  (loop (+ end 1)))))))))))))

(def (scan-line-comment source start prefixes)
  (let (prefix (scan-longest-literal source start prefixes))
    (and prefix
         (scan-while source (+ start (string-length prefix))
                     (lambda (ch) (not (newline? ch)))))))

;; scan-block-comment
;;   : (-> String Fixnum String String Fixnum)
;;   | doc m%
;;       `scan-block-comment` scans one non-nested delimiter pair.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-block-comment "/* value */" 0 "/*" "*/")
;;       ;; => 11
;;       ```
;;     %
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

;; scan-nested-block-comment
;;   : (-> String Fixnum String String Fixnum)
;;   | doc m%
;;       `scan-nested-block-comment` tracks balanced nested delimiters.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-nested-block-comment "/* /* */ */" 0 "/*" "*/")
;;       ;; => 11
;;       ```
;;     %
(def (scan-nested-block-comment source start opening closing)
  (let ((length (string-length source))
        (opening-length (string-length opening))
        (closing-length (string-length closing)))
    (and (literal-at? source start opening)
         (let loop ((offset (+ start opening-length)) (depth 1))
           (cond
            ((>= offset length) #f)
            ((literal-at? source offset opening)
             (loop (+ offset opening-length) (+ depth 1)))
            ((literal-at? source offset closing)
             (let ((next (+ offset closing-length))
                   (remaining (- depth 1)))
               (if (zero? remaining) next (loop next remaining))))
            (else (loop (+ offset 1) depth)))))))

(def (literal-at? source start literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ start literal-length) source-length)
         (let loop ((index 0))
           (or (= index literal-length)
               (and (char=? (string-ref source (+ start index))
                            (string-ref literal index))
                    (loop (+ index 1))))))))

;;; Compile a static literal catalog into a character trie once. The returned
;;; scanner follows at most the matching source prefix, independent of catalog
;;; size, and retains the longest terminal seen along that path.
;; : (-> (List String) (-> String Nat (Maybe Nat)))
(def (make-literal-end-scanner literals)
  (def (make-node) (vector #f (make-table test: eqv?)))
  (def (insert! root literal)
    (unless (and (string? literal) (positive? (string-length literal)))
      (error "lexer literals must be non-empty strings" literal))
    (let loop ((node root) (index 0))
      (if (= index (string-length literal))
        (vector-set! node 0 #t)
        (let* ((children (vector-ref node 1))
               (character (string-ref literal index))
               (child (table-ref children character #f)))
          (unless child
            (set! child (make-node))
            (table-set! children character child))
          (loop child (+ index 1))))))
  (let (root (make-node))
    (for-each (cut insert! root <>) literals)
    (lambda (source start)
      (let (source-length (string-length source))
        (let loop ((node root) (offset start) (selected #f))
          (if (= offset source-length)
            selected
            (let (child
                  (table-ref (vector-ref node 1)
                             (string-ref source offset) #f))
              (if child
                (let (next (+ offset 1))
                  (loop child next
                        (if (vector-ref child 0) next selected)))
                selected))))))))

;;; Merge literal-only lexical rules into one trie shared by every LR mode.
;;; Entries carry (literal name precedence declaration-index); the optional
;;; admitted vector filters complete rules without copying the trie per mode.
(def (make-ranked-literal-scanner entries)
  (def (make-node) (vector '() (make-table test: eqv?)))
  (def (best-admitted candidates admitted)
    (let loop ((remaining candidates) (best #f))
      (if (null? remaining)
        best
        (let (candidate (car remaining))
          (loop
           (cdr remaining)
           (if (and (or (not admitted)
                        (vector-ref admitted (caddr candidate)))
                    (or (not best)
                        (> (cadr candidate) (cadr best))
                        (and (= (cadr candidate) (cadr best))
                             (< (caddr candidate) (caddr best)))))
             candidate
             best))))))
  (let (root (make-node))
    (for-each
     (lambda (entry)
       (let ((literal (car entry))
             (name (cadr entry))
             (rank (caddr entry))
             (ordinal (cadddr entry)))
         (unless (and (string? literal)
                      (positive? (string-length literal)))
           (error "lexer literals must be non-empty strings" literal))
         (let insert ((node root) (index 0))
           (if (= index (string-length literal))
             (vector-set! node 0
                          (cons (list name rank ordinal)
                                (vector-ref node 0)))
             (let* ((children (vector-ref node 1))
                    (character (string-ref literal index))
                    (child (table-ref children character #f)))
               (unless child
                 (set! child (make-node))
                 (table-set! children character child))
               (insert child (+ index 1)))))))
     entries)
    (lambda (source start (admitted #f))
      (let (source-length (string-length source))
        (let scan ((node root) (offset start) (selected #f))
          (if (= offset source-length)
            selected
            (let (child
                  (table-ref (vector-ref node 1)
                             (string-ref source offset) #f))
              (if child
                (let* ((next (+ offset 1))
                       (terminal
                        (best-admitted (vector-ref child 0) admitted)))
                  (scan child next
                        (if terminal
                          (list (car terminal) next
                                (cadr terminal) (caddr terminal))
                          selected)))
                selected))))))))

;; scan-longest-literal
;;   : (-> String Fixnum List String)
;;   | doc m%
;;       `scan-longest-literal` selects the longest matching declared literal.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-longest-literal "<=" 0 '("<" "<="))
;;       ;; => "<="
;;       ```
;;     %
(def (scan-longest-literal source start literals)
  (foldl (lambda (candidate selected)
           (if (and (literal-at? source start candidate)
                    (or (not selected)
                        (> (string-length candidate)
                           (string-length selected))))
             candidate
             selected))
         #f
         literals))

(def (scan-emit source kind start end byte-start)
  (let* ((lexeme (substring source start end))
         (byte-end (+ byte-start (u8vector-length (string->utf8 lexeme)))))
    (make-token kind lexeme byte-start byte-end)))
