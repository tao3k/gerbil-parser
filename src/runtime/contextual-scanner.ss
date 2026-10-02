;;; -*- Gerbil -*-
;;; Generic executor for validated contextual scanner IR. All recognizers are
;;; closed engine opcodes; a language contributes data, never a scan callback.

(import (only-in ./scan
                 scan-balanced-word scan-horizontal-whitespace scan-identifier
                 scan-line scan-longest-literal scan-newline
                 scan-quoted-strings)
        (only-in ./token make-token)
        (only-in ./identity sha256-text))
(export +contextual-scanner-opcode-contract+
        prepare-contextual-scanner
        contextual-scanner-initial-state
        contextual-scanner-step
        contextual-scan-state?
        contextual-scan-state-character-offset
        contextual-scan-state-byte-offset
        contextual-scan-state-mode
        contextual-scan-state-with-mode
        contextual-scan-state-canonical
        restore-contextual-scan-state)

;;; Compiler products bind this semantic opcode contract. Changing opcode
;;; meaning requires a new contract, independently of lookup optimizations.
(def +contextual-scanner-opcode-contract+
  "gerbil-parser.contextual-scanner-opcodes.v1")

(defstruct contextual-scanner (source digest initial-mode modes positions rules cells)
  transparent: #t)
(defstruct contextual-scan-state
  (scanner character-offset byte-offset mode pending active expecting)
  transparent: #t)
(defstruct runtime-scan-rule (name mode form matcher rank action)
  transparent: #t)
(defstruct scan-match (rule end) transparent: #t)
(defstruct delimiter-obligation (marker strip-tabs? quoted?)
  transparent: #t)

(def (ir-ref ir key)
  (let (row (assq key ir)) (and row (cdr row))))

(def (canonical value)
  (call-with-output-string (lambda (port) (write value port))))

(def (scanner-ir-digest-valid? ir)
  (and (list? ir)
       (string? (ir-ref ir 'digest))
       (equal?
        (sha256-text
         (canonical (filter (lambda (row) (not (eq? (car row) 'digest)))
                            ir)))
        (ir-ref ir 'digest))))

(def (decode-rule row)
  (match row
    ([name mode form matcher rank action]
     (make-runtime-scan-rule name mode form matcher rank action))
    (else (error "invalid contextual scanner rule" row))))

(def (index-rules rows)
  (let (index (make-table test: eq?))
    (for-each
     (lambda (row)
       (let* ((rule (decode-rule row))
              (mode (runtime-scan-rule-mode rule)))
         (table-set! index mode
                     (cons rule (table-ref index mode '())))))
     (reverse rows))
    index))

(def (index-cells cells)
  (let ((index (make-table test: equal?))
        (missing (gensym 'missing-cell)))
    (for-each
     (lambda (cell)
       (let* ((key (take cell 3))
              (result (list-ref cell 3)))
         (unless (eq? (table-ref index key missing) missing)
           (error "duplicate contextual scanner dispatch cell" key))
         (table-set! index key (and result (car result)))))
     cells)
    index))

(def (prepare-contextual-scanner ir source)
  (unless (and (string? source)
               (equal? (ir-ref ir 'schema)
                       "gerbil-parser.contextual-scanner-ir.v1")
               (equal? (ir-ref ir 'opcode-contract)
                       +contextual-scanner-opcode-contract+)
               (scanner-ir-digest-valid? ir))
    (error "contextual scanner requires compiled IR and source"))
  (make-contextual-scanner
   source (ir-ref ir 'digest) (ir-ref ir 'initial-mode)
   (ir-ref ir 'modes) (ir-ref ir 'positions)
   (index-rules (ir-ref ir 'rules)) (index-cells (ir-ref ir 'cells))))

(def (contextual-scanner-initial-state scanner)
  (make-contextual-scan-state scanner 0 0
                              (contextual-scanner-initial-mode scanner)
                              '() #f #f))

(def (contextual-scan-state-with-mode state mode)
  (unless (and (contextual-scan-state? state)
               (memq mode
                     (contextual-scanner-modes
                      (contextual-scan-state-scanner state))))
    (error "unknown contextual scanner mode" mode))
  (make-contextual-scan-state
   (contextual-scan-state-scanner state)
   (contextual-scan-state-character-offset state)
   (contextual-scan-state-byte-offset state)
   mode
   (contextual-scan-state-pending state)
   (contextual-scan-state-active state)
   (contextual-scan-state-expecting state)))

(def (obligation-row obligation)
  (list (delimiter-obligation-marker obligation)
        (delimiter-obligation-strip-tabs? obligation)
        (delimiter-obligation-quoted? obligation)))

;;; The canonical checkpoint binds source and machine digests, but contains no
;;; closure, pointer, or POO object. A future resume operation can validate
;;; these fields before rebuilding its request-local scanner state.
(def (contextual-scan-state-canonical state)
  (let (scanner (contextual-scan-state-scanner state))
    (list (cons 'schema "gerbil-parser.contextual-scan-state.v1")
          (cons 'scannerDigest (contextual-scanner-digest scanner))
          (cons 'sourceDigest (sha256-text (contextual-scanner-source scanner)))
          (cons 'characterOffset
                (contextual-scan-state-character-offset state))
          (cons 'byteOffset (contextual-scan-state-byte-offset state))
          (cons 'mode (contextual-scan-state-mode state))
          (cons 'pending
                (map obligation-row (contextual-scan-state-pending state)))
          (cons 'active
                (and (contextual-scan-state-active state)
                     (obligation-row (contextual-scan-state-active state))))
          (cons 'expecting (contextual-scan-state-expecting state)))))

(def (receipt-ref receipt key)
  (let (row (assq key receipt)) (and row (cdr row))))

(def (restore-obligation row)
  (match row
    ([marker strip-tabs? quoted?]
     (unless (and (string? marker)
                  (boolean? strip-tabs?) (boolean? quoted?)
                  (or (> (string-length marker) 0) quoted?))
       (error "invalid scanner delimiter obligation" row))
     (make-delimiter-obligation marker strip-tabs? quoted?))
    (else (error "invalid scanner delimiter obligation" row))))

(def (restore-contextual-scan-state scanner receipt)
  (unless (and (contextual-scanner? scanner)
               (list? receipt)
               (equal? (receipt-ref receipt 'schema)
                       "gerbil-parser.contextual-scan-state.v1")
               (equal? (receipt-ref receipt 'scannerDigest)
                       (contextual-scanner-digest scanner))
               (equal? (receipt-ref receipt 'sourceDigest)
                       (sha256-text (contextual-scanner-source scanner))))
    (error "contextual scanner checkpoint identity mismatch"))
  (let* ((source (contextual-scanner-source scanner))
         (character-offset (receipt-ref receipt 'characterOffset))
         (byte-offset (receipt-ref receipt 'byteOffset))
         (mode (receipt-ref receipt 'mode))
         (pending-rows (receipt-ref receipt 'pending))
         (active-row (receipt-ref receipt 'active))
         (expecting (receipt-ref receipt 'expecting)))
    (unless (and (integer? character-offset)
                 (<= 0 character-offset (string-length source))
                 (integer? byte-offset)
                 (= byte-offset
                    (u8vector-length
                     (string->utf8 (substring source 0 character-offset))))
                 (memq mode (contextual-scanner-modes scanner))
                 (list? pending-rows)
                 (memq expecting '(#f plain strip-tabs)))
      (error "invalid contextual scanner checkpoint" receipt))
    (make-contextual-scan-state
     scanner character-offset byte-offset mode
     (map restore-obligation pending-rows)
     (and active-row (restore-obligation active-row))
     expecting)))

(def (literal-end source start value)
  (let (end (+ start (string-length value)))
    (and (<= end (string-length source))
         (string=? (substring source start end) value)
         end)))

(def (single-newline-end source start)
  (and (< start (string-length source))
       (case (string-ref source start)
         ((#\newline) (+ start 1))
         ((#\return)
          (if (and (< (+ start 1) (string-length source))
                   (char=? (string-ref source (+ start 1)) #\newline))
            (+ start 2)
            (+ start 1)))
         (else #f))))

(def (line-content-end source start end)
  (cond
   ((and (> end (+ start 1))
         (char=? (string-ref source (- end 2)) #\return)
         (char=? (string-ref source (- end 1)) #\newline))
    (- end 2))
   ((and (> end start)
         (or (char=? (string-ref source (- end 1)) #\return)
             (char=? (string-ref source (- end 1)) #\newline)))
    (- end 1))
   (else end)))

(def (marker-line-end source start state)
  (let (active (contextual-scan-state-active state))
    (and active
         (let* ((end (scan-line source start))
                (content-end (and end (line-content-end source start end)))
                (content-start
                 (if (delimiter-obligation-strip-tabs? active)
                   (let loop ((cursor start))
                     (if (and (< cursor content-end)
                              (char=? (string-ref source cursor) #\tab))
                       (loop (+ cursor 1)) cursor))
                   start)))
           (and end
                (string=?
                 (substring source content-start content-end)
                 (delimiter-obligation-marker active))
                end)))))

(def (matcher-end source start expression state)
  (match expression
    (['literal value] (literal-end source start value))
    (['literals values]
     (let (found (scan-longest-literal source start values))
       (and found (+ start (string-length found)))))
    (['horizontal-whitespace+]
     (scan-horizontal-whitespace source start))
    (['newline] (scan-newline source start))
    (['newline-one] (single-newline-end source start))
    (['identifier] (scan-identifier source start))
    (['marker-line] (marker-line-end source start state))
    (['body-line]
     (and (contextual-scan-state-active state)
          (scan-line source start)))
    (['quoted-string delimiters]
     (scan-quoted-strings source start delimiters))
    (['balanced-word stops quotes pairs]
     (scan-balanced-word source start stops quotes pairs))
    (else (error "unknown contextual scanner opcode" expression))))

(def (prefer-match current candidate)
  (cond
   ((not current) candidate)
   ((> (scan-match-end candidate) (scan-match-end current)) candidate)
   ((< (scan-match-end candidate) (scan-match-end current)) current)
   ((> (runtime-scan-rule-rank (scan-match-rule candidate))
       (runtime-scan-rule-rank (scan-match-rule current)))
    candidate)
   ((< (runtime-scan-rule-rank (scan-match-rule candidate))
       (runtime-scan-rule-rank (scan-match-rule current)))
    current)
   ((and (eq? (runtime-scan-rule-form (scan-match-rule candidate))
              (runtime-scan-rule-form (scan-match-rule current)))
         (equal? (runtime-scan-rule-action (scan-match-rule candidate))
                 (runtime-scan-rule-action (scan-match-rule current))))
    current)
   (else
    (error "ambiguous contextual scanner match"
           (runtime-scan-rule-name (scan-match-rule current))
           (runtime-scan-rule-name (scan-match-rule candidate))))))

(def (best-match scanner state)
  (foldl
   (lambda (rule current)
       (let (end (matcher-end (contextual-scanner-source scanner)
                               (contextual-scan-state-character-offset state)
                               (runtime-scan-rule-matcher rule) state))
         (if end (prefer-match current (make-scan-match rule end)) current)))
   #f
   (table-ref (contextual-scanner-rules scanner)
              (contextual-scan-state-mode state) '())))

(def (result-kind scanner mode position form)
  (let (kind (table-ref (contextual-scanner-cells scanner)
                        (list mode position form) #f))
    (unless kind
      (error "missing contextual scanner dispatch"
             mode position form))
    kind))

(def (decode-shell-delimiter word)
  (let (length (string-length word))
    (let loop ((offset 0) (quote-mode #f) (quoted? #f) (characters '()))
      (if (= offset length)
        (begin
          (when quote-mode
            (error "unterminated delimiter quote" word))
          (values (list->string (reverse characters)) quoted?))
        (let (character (string-ref word offset))
          (cond
           ((and (not (eq? quote-mode 'single))
                 (char=? character #\\))
            (if (< (+ offset 1) length)
              (let (escaped (string-ref word (+ offset 1)))
                (cond
                 ((char=? escaped #\newline)
                  (loop (+ offset 2) quote-mode quoted? characters))
                 ((and (eq? quote-mode 'double)
                       (not (memv escaped '(#\$ #\` #\" #\\))))
                  (loop (+ offset 2) quote-mode quoted?
                        (cons escaped (cons character characters))))
                 (else
                  (loop (+ offset 2) quote-mode #t
                        (cons escaped characters)))))
              (loop (+ offset 1) quote-mode #t
                    (cons character characters))))
           ((and (not (eq? quote-mode 'double))
                 (char=? character #\'))
            (loop (+ offset 1)
                  (if quote-mode #f 'single) #t characters))
           ((and (not (eq? quote-mode 'single))
                 (char=? character #\"))
            (loop (+ offset 1)
                  (if quote-mode #f 'double) #t characters))
           (else
            (loop (+ offset 1) quote-mode quoted?
                  (cons character characters)))))))))

(def (decode-marker policy word strip-tabs?)
  (let-values (((marker quoted?)
                (if (eq? policy 'raw)
                  (values word #f)
                  (decode-shell-delimiter word))))
    (when (and (string=? marker "") (not quoted?))
      (error "empty deferred delimiter" word))
    (make-delimiter-obligation marker strip-tabs? quoted?)))

(def (updated-state state mode pending active expecting)
  (make-contextual-scan-state
   (contextual-scan-state-scanner state)
   (contextual-scan-state-character-offset state)
   (contextual-scan-state-byte-offset state)
   mode pending active expecting))

(def (apply-scan-action action lexeme state)
  (let ((mode (contextual-scan-state-mode state))
        (pending (contextual-scan-state-pending state))
        (active (contextual-scan-state-active state))
        (expecting (contextual-scan-state-expecting state)))
    (match action
      ('keep state)
      (['expect-marker strip-tabs?]
       (when expecting
         (error "deferred delimiter already expected"))
       (updated-state state mode pending active
                      (if strip-tabs? 'strip-tabs 'plain)))
      (['enqueue-if-expecting policy]
       (if expecting
         (updated-state
          state mode
          (append pending
                  (list (decode-marker policy lexeme
                                       (eq? expecting 'strip-tabs))))
          active #f)
         state))
      (['activate-next body-mode]
       (when expecting
         (error "missing deferred delimiter before newline"))
       (if (pair? pending)
         (updated-state state body-mode (cdr pending) (car pending) #f)
         state))
      (['finish-marker base-mode body-mode]
       (unless active
         (error "deferred delimiter closed without an active marker"))
       (if (pair? pending)
         (updated-state state body-mode (cdr pending) (car pending) #f)
         (updated-state state base-mode '() #f #f)))
      (else (error "unknown contextual scan action" action)))))

;;; A checkpoint is immutable and bound to the prepared scanner. Parser
;;; reductions may change only its declared mode, without changing offsets.
(def (contextual-scanner-step scanner state position)
  (unless (and (contextual-scan-state? state)
               (eq? (contextual-scan-state-scanner state) scanner)
               (memq position (contextual-scanner-positions scanner)))
    (error "invalid contextual scanner checkpoint or position"))
  (let* ((source (contextual-scanner-source scanner))
         (start (contextual-scan-state-character-offset state))
         (byte-start (contextual-scan-state-byte-offset state))
         (mode (contextual-scan-state-mode state)))
    (if (= start (string-length source))
      (begin
        (when (or (contextual-scan-state-active state)
                  (pair? (contextual-scan-state-pending state))
                  (contextual-scan-state-expecting state))
          (error "unfinished contextual scanner obligation"))
        (values #f state))
      (let (match (best-match scanner state))
        (unless match
          (error "contextual scanner has no match" mode start))
        (let* ((rule (scan-match-rule match))
               (end (scan-match-end match))
               (lexeme (substring source start end))
               (byte-end (+ byte-start
                            (u8vector-length (string->utf8 lexeme))))
               (kind (result-kind scanner mode position
                                  (runtime-scan-rule-form rule))))
          (unless (< start end)
            (error "contextual scanner did not advance" mode start))
          (values
           (make-token kind lexeme byte-start byte-end)
           (apply-scan-action
            (runtime-scan-rule-action rule) lexeme
            (make-contextual-scan-state
             scanner end byte-end mode
             (contextual-scan-state-pending state)
             (contextual-scan-state-active state)
             (contextual-scan-state-expecting state)))))))))
