;;; -*- Gerbil -*-
;;; Generic executor for validated contextual scanner IR. All recognizers are
;;; closed engine opcodes; a language contributes data, never a scan callback.

(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ./scan
                 make-literal-end-scanner identifier-start? horizontal-whitespace? newline?
                 scan-balanced-word scan-horizontal-whitespace scan-identifier
                 scan-line scan-longest-literal scan-newline
                 scan-quoted-strings)
        (only-in ./region-scanner prepare-region-plan region-plan-end)
        (only-in ./token make-token token-kind token-start token-end)
        (only-in ./identity sha256-text))
(export +empty-delimiter-queue+
        delimiter-queue-empty? delimiter-queue-list
        delimiter-queue-enqueue delimiter-queue-take
        decode-marker delimiter-obligation?
        delimiter-obligation-marker delimiter-obligation-strip-tabs? delimiter-obligation-quoted?
        +contextual-scanner-opcode-contract+
        prepare-contextual-scanner
        prepare-contextual-scanner-plan
        contextual-scanner-initial-state
        contextual-scanner-step
        contextual-scan-state?
        contextual-scan-state-character-offset
        contextual-scan-state-byte-offset
        contextual-scan-state-mode
        contextual-scan-state-with-mode
        contextual-scan-state-canonical
        contextual-scan-state-converged?
        contextual-scanner-replay-prefix

        contextual-scan-state-source-length contextual-scan-state-common-suffix-length
        prepare-contextual-scan-suffix contextual-scan-suffix-state
        restore-contextual-scan-state)

;;; Compiler products bind this semantic opcode contract. Changing opcode
;;; meaning requires a new contract, independently of lookup optimizations.
(def +contextual-scanner-opcode-contract+
  "gerbil-parser.contextual-scanner-opcodes.v1")

(defstruct contextual-scanner (source source-identity digest initial-mode modes positions rules cells plan-owner)
  transparent: #t)
;;; Static indexes are owned by the opaque plan and read only after admission.
(defstruct contextual-scanner-plan
  (digest initial-mode modes positions short-rules long-rules cells))

(defstruct contextual-scan-state
  (scanner character-offset byte-offset mode pending active expecting)
  transparent: #t)
(defstruct runtime-scan-rule (name mode form matcher rank action)
  transparent: #t)
(defstruct scan-match (rule end) transparent: #t)
(defstruct delimiter-obligation (marker strip-tabs? quoted?)
  transparent: #t)

;;; Immutable FIFO: front is ordered, rear is reversed. Enqueue never copies
;;; the pending batch. A linear scan reverses each rear at most once; replaying
;;; an old checkpoint can repeat that work, so no branching amortized bound is
;;; claimed. Canonical checkpoints retain their ordered list representation.
(defstruct delimiter-queue (front rear) transparent: #t)
(def +empty-delimiter-queue+ (make-delimiter-queue '() '()))

(def (delimiter-queue-empty? queue)
  (and (null? (delimiter-queue-front queue))
       (null? (delimiter-queue-rear queue))))

(def (delimiter-queue-list queue)
  (append (delimiter-queue-front queue) (reverse (delimiter-queue-rear queue))))

(def (delimiter-queue-enqueue queue obligation)
  (unless (delimiter-obligation? obligation)
    (error "invalid deferred delimiter obligation" obligation))
  (make-delimiter-queue (delimiter-queue-front queue)
                        (cons obligation (delimiter-queue-rear queue))))

(def (delimiter-queue-take queue)
  (when (delimiter-queue-empty? queue) (error "empty deferred delimiter queue"))
  (let* ((front (delimiter-queue-front queue))
         (rear (delimiter-queue-rear queue))
         (ready (if (null? front) (reverse rear) front)))
    (values (car ready)
            (make-delimiter-queue (cdr ready) (if (null? front) '() rear)))))

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

;;; Trie construction pays off for larger catalogs scanned over longer sources.
;;; Keep small catalogs and short one-shot inputs on the linear path.
(def (decode-rule row literal-cache source-length)
  (match row
    ([name mode form matcher rank action]
     (let (prepared
           (match matcher
             (['literals values]
              (if (and (>= source-length 64) (>= (length values) 128))
                (let (scanner (or (table-ref literal-cache values #f)
                                  (let (scanner (make-literal-end-scanner values))
                                    (table-set! literal-cache values scanner)
                                    scanner)))
                  (list 'literal-trie scanner values))
                matcher))
             (['region-word spec] (list 'prepared-region (prepare-region-plan spec)))
             (['unless-prefix prefixes exceptions child]
              (match child
                (['region-word spec]
                 (list 'unless-prefix prefixes exceptions (list 'prepared-region (prepare-region-plan spec))))
                ((or ['literal-trie . _] ['prepared-region . _])
                 (error "private contextual scanner matcher in input IR"))
                (['unless-prefix . _] (error "nested contextual scanner guard in input IR"))
                (else matcher)))
             ((or ['literal-trie . _] ['prepared-region . _])
              (error "private contextual scanner matcher in input IR"))
             (else matcher)))
       (make-runtime-scan-rule name mode form prepared rank action)))
    (else (error "invalid contextual scanner rule" row))))

(def (index-rules rows source-length (prefix-index? #f))
  (let ((index (make-table test: eq?))
        (literal-cache (make-table test: equal?)))
    (for-each
     (lambda (row)
       (let* ((rule (decode-rule row literal-cache source-length))
              (mode (runtime-scan-rule-mode rule)))
         (table-set! index mode
                     (cons rule (table-ref index mode '())))))
     (reverse rows))
    (when prefix-index?
      ;; Publish only after the entire ordered mode row is prepared.
      (for-each (lambda (entry)
                  (table-set! index (car entry) (prepare-first-character-rules (cdr entry))))
                (table->list index)))
    index))

;;; Prefix pruning admits every rule that could match. Stateful and unknown
;;; opcode families remain universal candidates, retaining their executor and
;;; source-order tie/ambiguity semantics. No candidate list is built per token.
(defstruct first-character-rules (ascii wide alpha other))
(def (literal-first-characters values)
  (if (any (lambda (value) (zero? (string-length value))) values)
    'any
    (let (characters (make-table test: eqv?))
      (for-each (lambda (value) (table-set! characters (string-ref value 0) #t)) values)
      characters)))
(def (rule-first-characters rule)
  (match (runtime-scan-rule-matcher rule)
    (['literal value] (literal-first-characters (list value)))
    (['literals values] (literal-first-characters values))
    (['literal-trie _ values] (literal-first-characters values))
    (['quoted-string delimiters] (literal-first-characters delimiters))
    (['identifier] 'identifier)
    (['horizontal-whitespace+] 'horizontal)
    ((or ['newline] ['newline-one]) 'newline)
    (else 'any)))
(def (first-character-admits? hint character)
  (case hint
    ((any) #t)
    ((identifier) (identifier-start? character))
    ((horizontal) (horizontal-whitespace? character))
    ((newline) (newline? character))
    (else (table-ref hint character #f))))
(def (merge-ordered-rule-references left right)
  (cond ((null? left) right)
        ((null? right) left)
        ((< (caar left) (caar right))
         (cons (car left) (merge-ordered-rule-references (cdr left) right)))
        (else (cons (car right) (merge-ordered-rule-references left (cdr right))))))
(def (prepare-first-character-rules rules)
  (let* ((references (map cons (iota (length rules)) rules))
         (hints (map (lambda (reference)
                       (cons reference (rule-first-characters (cdr reference)))) references))
         (ascii (make-vector 128 '()))
         (wide (make-table test: eqv?))
         (alpha (map car (filter (lambda (entry) (memq (cdr entry) '(any identifier))) hints)))
         (other (map car (filter (lambda (entry) (eq? (cdr entry) 'any)) hints))))
    (let loop ((code 0))
      (when (< code 128)
        (let (character (integer->char code))
          (vector-set! ascii code
                       (map (lambda (entry) (cdar entry))
                            (filter (lambda (entry) (first-character-admits? (cdr entry) character)) hints))))
        (loop (+ code 1))))
    ;; Each finite prefix is inserted once per rule. Universal/class rows are
    ;; merged in original order; Unicode prefix construction avoids U*R scans.
    (for-each
     (lambda (entry)
       (let ((reference (car entry)) (hint (cdr entry)))
         (unless (symbol? hint)
           (for-each
            (lambda (prefix)
              (let (character (car prefix))
                (when (>= (char->integer character) 128)
                  (table-set! wide character
                              (cons reference (table-ref wide character '()))))))
            (table->list hint)))))
     (reverse hints))
    (for-each
     (lambda (entry)
       (let ((character (car entry)) (specific (cdr entry)))
         (table-set! wide character
                     (map cdr (merge-ordered-rule-references
                               (if (identifier-start? character) alpha other) specific)))))
     (table->list wide))
    (make-first-character-rules ascii wide (map cdr alpha) (map cdr other))))
(def (first-character-candidates index character)
  (let (code (char->integer character))
    (if (< code 128)
      (vector-ref (first-character-rules-ascii index) code)
      (or (table-ref (first-character-rules-wide index) character #f)
          (if (identifier-start? character)
            (first-character-rules-alpha index)
            (first-character-rules-other index))))))

(def (index-cells/hash cells)
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

(defstruct grouped-dispatch (modes))

(def (multiple-dispatch-rows? cells)
  (and (pair? cells)
       (let ((mode (caar cells)) (position (cadar cells)))
         (any (lambda (cell) (or (not (eq? mode (car cell)))
                                 (not (eq? position (cadr cell)))))
              (cdr cells)))))

(def (short-symbolic-cells? cells)
  (let loop ((rest cells) (forms '()))
    (or (null? rest)
        (let* ((cell (car rest)) (form (caddr cell)))
          (and (symbol? (car cell)) (symbol? (cadr cell)) (symbol? form)
               (let (next (if (memq form forms) forms (cons form forms)))
                 (and (<= (length next) 8) (loop (cdr rest) next))))))))

;;; Factor the composite key into mode/position indexes and short form rows.
;;; Multiple rows and at most eight forms bound row search. Single rows, wide
;;; form catalogs, and structural keys retain the original hash path.
(def (index-cells cells)
  (if (and (multiple-dispatch-rows? cells) (short-symbolic-cells? cells))
    (let (modes (make-table test: eq?))
      (for-each
       (lambda (cell)
         (let* ((mode (car cell)) (position (cadr cell)) (form (caddr cell))
                (positions (or (table-ref modes mode #f)
                               (let (index (make-table test: eq?))
                                 (table-set! modes mode index) index)))
                (row (table-ref positions position '()))
                (result (list-ref cell 3)))
           (when (assq form row)
             (error "duplicate contextual scanner dispatch cell" (take cell 3)))
           (table-set! positions position
                       (cons (cons form (and result (car result))) row))))
       cells)
      (make-grouped-dispatch modes))
    (index-cells/hash cells)))

(def (validate-scanner-ir! ir)
  (unless (and (equal? (ir-ref ir 'schema)
                       "gerbil-parser.contextual-scanner-ir.v1")
               (equal? (ir-ref ir 'opcode-contract)
                       +contextual-scanner-opcode-contract+)
               (scanner-ir-digest-valid? ir))
    (error "contextual scanner requires compiled IR and source")))

;;; Preserve DAG sharing and symbol identities while taking ownership of every
;;; pair and string. Cycles and non-data values cannot enter a reusable plan.
(def (snapshot-scanner-ir ir)
  (let ((copies (make-table test: eq?)) (active (make-table test: eq?)))
    (def (copy value)
      (cond
       ((pair? value)
        (when (table-ref active value #f)
          (error "cyclic contextual scanner plan IR"))
        (or (table-ref copies value #f)
            (begin
              (table-set! active value #t)
              (let (result (cons (copy (car value)) (copy (cdr value))))
                (table-set! active value #f)
                (table-set! copies value result)
                result))))
       ((string? value)
        (or (table-ref copies value #f)
            (let (result (string-copy value))
              (table-set! copies value result) result)))
       ((or (null? value) (symbol? value) (number? value)
            (boolean? value) (char? value)) value)
       (else (error "contextual scanner plan requires closed IR data"))))
    (copy ir)))

(def (prepare-contextual-scanner-plan ir)
  (let (owned (snapshot-scanner-ir ir))
    (validate-scanner-ir! owned)
    (let* ((rules (ir-ref owned 'rules))
           (short-rules (index-rules rules 0 #t))
           (has-trie? (any (lambda (row)
                            (match (list-ref row 3)
                              (['literals values] (>= (length values) 128))
                              (else #f))) rules)))
      (make-contextual-scanner-plan
       (ir-ref owned 'digest) (ir-ref owned 'initial-mode)
       (ir-ref owned 'modes) (ir-ref owned 'positions)
       short-rules (if has-trie? (index-rules rules 64 #t) short-rules)
       (index-cells (ir-ref owned 'cells))))))

(def (prepare-contextual-scanner ir source)
  (unless (string? source)
    (error "contextual scanner requires compiled IR and source"))
  ;; A request owns one source snapshot. Its identity is shared lazily by every
  ;; checkpoint; caller mutation cannot change scanning or invalidate that hash.
  (let* ((owned (string-copy source))
         (identity (delay (sha256-text owned))))
    (if (contextual-scanner-plan? ir)
      (make-contextual-scanner
       owned identity (string-copy (contextual-scanner-plan-digest ir))
       (contextual-scanner-plan-initial-mode ir)
       (contextual-scanner-plan-modes ir) (contextual-scanner-plan-positions ir)
       (if (< (string-length owned) 64)
         (contextual-scanner-plan-short-rules ir) (contextual-scanner-plan-long-rules ir))
       (contextual-scanner-plan-cells ir) ir)
      (begin
        (validate-scanner-ir! ir)
        (make-contextual-scanner
         owned identity (string-copy (ir-ref ir 'digest)) (ir-ref ir 'initial-mode)
         (ir-ref ir 'modes) (ir-ref ir 'positions)
         (index-rules (ir-ref ir 'rules) (string-length owned)) (index-cells (ir-ref ir 'cells)) (list 'request-local))))))

(def (contextual-scanner-initial-state scanner)
  (make-contextual-scan-state scanner 0 0
                              (contextual-scanner-initial-mode scanner)
                              +empty-delimiter-queue+ #f #f))

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

(def (contextual-scan-state-source-length state)
  (string-length (contextual-scanner-source (contextual-scan-state-scanner state))))

;;; Alignment reads only engine-owned snapshots, never caller-mutable strings.
(def (contextual-scan-state-common-suffix-length before after)
  (let ((left (contextual-scanner-source (contextual-scan-state-scanner before)))
        (right (contextual-scanner-source (contextual-scan-state-scanner after))))
    (let loop ((i (- (string-length left) 1)) (j (- (string-length right) 1)) (count 0))
      (if (and (>= i 0) (>= j 0) (char=? (string-ref left i) (string-ref right j)))
        (loop (- i 1) (- j 1) (+ count 1)) count))))

;;; Opaque proof binds two independently reached boundaries. The source worker
;;; supplies the same fixed parser position for the old and new continuations.
(defstruct contextual-scan-suffix (before after))
(def (prepare-contextual-scan-suffix before after)
  (and (contextual-scan-state-converged? before after)
       (make-contextual-scan-suffix before after)))

;;; Only states from the admitted old continuation may be rebound. Prefix
;;; checkpoints remain foreign; no matching-prefix restart is authorized here.
(def (contextual-scan-suffix-state suffix state)
  (unless (and (contextual-scan-suffix? suffix) (contextual-scan-state? state))
    (error "invalid contextual suffix state"))
  (let ((before (contextual-scan-suffix-before suffix))
        (after (contextual-scan-suffix-after suffix)))
    (unless (and (eq? (contextual-scan-state-scanner before) (contextual-scan-state-scanner state))
                 (<= (contextual-scan-state-character-offset before) (contextual-scan-state-character-offset state))
                 (<= (contextual-scan-state-byte-offset before) (contextual-scan-state-byte-offset state)))
      (error "checkpoint is outside the admitted scanner suffix"))
    (make-contextual-scan-state
     (contextual-scan-state-scanner after)
     (+ (contextual-scan-state-character-offset state)
        (- (contextual-scan-state-character-offset after) (contextual-scan-state-character-offset before)))
     (+ (contextual-scan-state-byte-offset state)
        (- (contextual-scan-state-byte-offset after) (contextual-scan-state-byte-offset before)))
     (contextual-scan-state-mode state) (contextual-scan-state-pending state)
     (contextual-scan-state-active state) (contextual-scan-state-expecting state))))

(def (same-delimiter-obligation? left right)
  (or (eq? left right)
      (and left right
           (string=? (delimiter-obligation-marker left) (delimiter-obligation-marker right))
           (eq? (delimiter-obligation-strip-tabs? left) (delimiter-obligation-strip-tabs? right))
           (eq? (delimiter-obligation-quoted? left) (delimiter-obligation-quoted? right)))))

(def (same-scanner-context? before after)
  (and
   (eq? (contextual-scanner-plan-owner (contextual-scan-state-scanner before))
        (contextual-scanner-plan-owner (contextual-scan-state-scanner after)))
   (eq? (contextual-scan-state-mode before) (contextual-scan-state-mode after))
   (eq? (contextual-scan-state-expecting before) (contextual-scan-state-expecting after))
   (same-delimiter-obligation? (contextual-scan-state-active before)
                              (contextual-scan-state-active after))
   (let loop ((a (delimiter-queue-list (contextual-scan-state-pending before)))
              (b (delimiter-queue-list (contextual-scan-state-pending after))))
     (if (null? a) (null? b)
       (and (pair? b) (same-delimiter-obligation? (car a) (car b))
            (loop (cdr a) (cdr b)))))))

;;; Re-execute the recorded parser positions on both owned sources. Equal text
;;; alone cannot certify decisions that inspected past the checkpoint boundary.
;;; Returns a newly reached checkpoint, never a relocated old state.
(def (contextual-scanner-replay-prefix scanner checkpoint positions)
  (unless (and (contextual-scan-state? checkpoint) (list? positions))
    (error "scanner prefix replay requires a checkpoint and position schedule"))
  (let* ((old (contextual-scan-state-scanner checkpoint))
         (left (contextual-scanner-source old))
         (right (contextual-scanner-source scanner))
         (end (contextual-scan-state-character-offset checkpoint)))
    (unless (and (eq? (contextual-scanner-plan-owner old) (contextual-scanner-plan-owner scanner))
                 (<= end (string-length right))
                 (let loop ((i 0))
                   (or (= i end) (and (char=? (string-ref left i) (string-ref right i))
                                     (loop (+ i 1))))))
      (error "scanner prefix source or plan mismatch"))
    (let loop ((rest positions) (a (contextual-scanner-initial-state old))
               (b (contextual-scanner-initial-state scanner)))
      (if (null? rest)
        (begin
          (unless (and (= (contextual-scan-state-character-offset a) end)
                       (= (contextual-scan-state-byte-offset a) (contextual-scan-state-byte-offset checkpoint))
                       (same-scanner-context? a checkpoint)
                       (same-scanner-context? a b))
            (error "scanner checkpoint not reproduced by position schedule"))
          b)
        (let-values (((token-a next-a) (contextual-scanner-step old a (car rest)))
                     ((token-b next-b) (contextual-scanner-step scanner b (car rest))))
          (unless (and (<= (contextual-scan-state-character-offset next-a) end)
                       (= (contextual-scan-state-character-offset next-a) (contextual-scan-state-character-offset next-b))
                       (if token-a
                         (and token-b (eq? (token-kind token-a) (token-kind token-b))
                              (= (token-start token-a) (token-start token-b))
                              (= (token-end token-a) (token-end token-b)))
                         (not token-b)))
            (error "scanner prefix lexical decision changed"))
          (loop (cdr rest) next-a next-b))))))

;;; Both states must have been reached independently. This proves identical
;;; future scanner decisions only under the same parser-position schedule;
;;; it neither relocates a checkpoint nor admits parser/subtree reuse.
(def (contextual-scan-state-converged? before after)
  (unless (and (contextual-scan-state? before) (contextual-scan-state? after))
    (error "scanner convergence requires two checkpoints"))
  (let* ((old (contextual-scan-state-scanner before))
         (new (contextual-scan-state-scanner after))
         (left (contextual-scanner-source old))
         (right (contextual-scanner-source new))
         (start-left (contextual-scan-state-character-offset before))
         (start-right (contextual-scan-state-character-offset after)))
    (and
     (same-scanner-context? before after)
     (= (- (string-length left) start-left) (- (string-length right) start-right))
     ;; Compare the owned suffix directly: no full source hash or suffix copy.
     (let loop ((i start-left) (j start-right))
       (or (= i (string-length left))
           (and (char=? (string-ref left i) (string-ref right j))
                (loop (+ i 1) (+ j 1))))))))

(def (obligation-row obligation)
  (list (string-copy (delimiter-obligation-marker obligation))
        (delimiter-obligation-strip-tabs? obligation)
        (delimiter-obligation-quoted? obligation)))

;;; The canonical checkpoint binds source and machine digests, but contains no
;;; closure, pointer, or POO object. Receipt strings belong to the recipient;
;;; restore validates identity and owns its reconstructed delimiter obligations.
(def (contextual-scan-state-canonical state)
  (let (scanner (contextual-scan-state-scanner state))
    (list (cons 'schema (string-copy "gerbil-parser.contextual-scan-state.v1"))
          (cons 'scannerDigest (string-copy (contextual-scanner-digest scanner)))
          (cons 'sourceDigest (string-copy (force (contextual-scanner-source-identity scanner))))
          (cons 'characterOffset
                (contextual-scan-state-character-offset state))
          (cons 'byteOffset (contextual-scan-state-byte-offset state))
          (cons 'mode (contextual-scan-state-mode state))
          (cons 'pending
                (map obligation-row
                     (delimiter-queue-list (contextual-scan-state-pending state))))
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
     (make-delimiter-obligation (string-copy marker) strip-tabs? quoted?))
    (else (error "invalid scanner delimiter obligation" row))))

(def (restore-contextual-scan-state scanner receipt)
  (unless (and (contextual-scanner? scanner)
               (list? receipt)
               (equal? (receipt-ref receipt 'schema)
                       "gerbil-parser.contextual-scan-state.v1")
               (equal? (receipt-ref receipt 'scannerDigest)
                       (contextual-scanner-digest scanner))
               (equal? (receipt-ref receipt 'sourceDigest)
                       (force (contextual-scanner-source-identity scanner))))
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
                    (string-utf8-length source 0 character-offset))
                 (memq mode (contextual-scanner-modes scanner))
                 (list? pending-rows)
                 (memq expecting '(#f plain strip-tabs)))
      (error "invalid contextual scanner checkpoint" receipt))
    (make-contextual-scan-state
     scanner character-offset byte-offset mode
     (make-delimiter-queue (map restore-obligation pending-rows) '())
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

(def (profile-line-end source start separator)
 (let loop ((at start))
  (cond ((= at (string-length source)) at)
        ((char=? (string-ref source at) separator) (+ at 1))
        (else (loop (+ at 1))))))
(def (profile-marker-end source start separator state)
 (let (active (contextual-scan-state-active state))
  (and active
   (let* ((end (profile-line-end source start separator))
          (content-end (if (and (> end start) (char=? (string-ref source (- end 1)) separator)) (- end 1) end))
          (content-start (if (delimiter-obligation-strip-tabs? active)
             (let loop ((at start)) (if (and (< at content-end) (char=? (string-ref source at) #\tab)) (loop (+ at 1)) at)) start)))
    (and (equal? (substring source content-start content-end) (delimiter-obligation-marker active)) end)))))
(def (matcher-end source start expression state)
  (match expression
    (['unless-prefix prefixes exceptions child]
     (and (or (any (lambda (prefix) (literal-end source start prefix)) exceptions)
              (not (any (lambda (prefix) (literal-end source start prefix)) prefixes)))
          (matcher-end source start child state)))
    (['line-prefix prefix separator]
     (and (literal-end source start prefix)
      (let* ((ch (string-ref separator 0)) (end (profile-line-end source start ch)))
       (if (and (> end start) (char=? (string-ref source (- end 1)) ch)) (- end 1) end))))
    (['marker-line-at separator] (profile-marker-end source start (string-ref separator 0) state))
    (['body-line-at separator]
     (and (contextual-scan-state-active state) (profile-line-end source start (string-ref separator 0))))
    (['literal value] (literal-end source start value))
    (['literal-trie scanner _values] (scanner source start))
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
    (['prepared-region plan] (region-plan-end plan source start))
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
   (let (rules (table-ref (contextual-scanner-rules scanner)
                          (contextual-scan-state-mode state) '()))
     (if (first-character-rules? rules)
       (first-character-candidates rules
         (string-ref (contextual-scanner-source scanner)
                     (contextual-scan-state-character-offset state)))
       rules))))

(def (result-kind scanner mode position form)
  (let* ((index (contextual-scanner-cells scanner))
         (kind (if (table? index)
                 (table-ref index (list mode position form) #f)
                 (let* ((positions (table-ref (grouped-dispatch-modes index) mode #f))
                        (row (and positions (table-ref positions position '())))
                        (cell (and row (assq form row))))
                   (and cell (cdr cell))))))
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
  (unless (and (memq policy '(raw shell-quote-removal))
               (string? word) (boolean? strip-tabs?))
    (error "invalid deferred delimiter declaration" policy word strip-tabs?))
  (let-values (((marker quoted?)
                (if (eq? policy 'raw)
                  (values word #f)
                  (decode-shell-delimiter word))))
    (when (and (string=? marker "") (not quoted?))
      (error "empty deferred delimiter" word))
    (make-delimiter-obligation (string-copy marker) strip-tabs? quoted?)))

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
      (['expect-marker-in strip-tabs? target-mode]
       (when expecting (error "deferred delimiter already expected"))
       (updated-state state target-mode pending active (if strip-tabs? 'strip-tabs 'plain)))
      (['enqueue-marker-in policy target-mode]
       (unless expecting (error "no deferred delimiter expected"))
       (updated-state state target-mode
        (delimiter-queue-enqueue pending (decode-marker policy lexeme (eq? expecting 'strip-tabs))) active #f))
      (['expect-marker strip-tabs?]
       (when expecting
         (error "deferred delimiter already expected"))
       (updated-state state mode pending active
                      (if strip-tabs? 'strip-tabs 'plain)))
      (['enqueue-if-expecting policy]
       (if expecting
         (updated-state
          state mode
          (delimiter-queue-enqueue
           pending (decode-marker policy lexeme (eq? expecting 'strip-tabs)))
          active #f)
         state))
      (['activate-next body-mode]
       (when expecting
         (error "missing deferred delimiter before newline"))
       (if (not (delimiter-queue-empty? pending))
         (let-values (((active rest) (delimiter-queue-take pending)))
           (updated-state state body-mode rest active #f))
         state))
      (['finish-marker base-mode body-mode]
       (unless active
         (error "deferred delimiter closed without an active marker"))
       (if (not (delimiter-queue-empty? pending))
         (let-values (((next rest) (delimiter-queue-take pending)))
           (updated-state state body-mode rest next #f))
         (updated-state state base-mode +empty-delimiter-queue+ #f #f)))
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
                  (not (delimiter-queue-empty? (contextual-scan-state-pending state)))
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
                            (string-utf8-length lexeme)))
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
