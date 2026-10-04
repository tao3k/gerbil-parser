;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ./parser-ir-data parser-ir-ref)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-parse parser-machine-runtime
                 parser-machine-trivia parser-machine-direct-drive
                 parser-machine-direct-source)
        (only-in ./artifact
                 +diagnostic-schema+ make-failure-parse-artifact
                 make-success-parse-artifact parse-artifact-success?)
        (only-in ./lexer lex-source lex-source-from scan-source-token)
        (only-in ./contextual-scanner
                 prepare-contextual-scanner prepare-contextual-scanner-plan contextual-scanner-initial-state
                 contextual-scanner-step
                 contextual-scan-state-character-offset
                 contextual-scan-state-byte-offset)
        (only-in ./identity sha256-text)
        (only-in ./lr-parser
                 lr-runtime-layout?
                 lr-checkpoint-deterministic-shifts lr-checkpoint-drive
                 lr-checkpoint-drive/contextual
                 lr-checkpoint-feed
                 lr-checkpoint-lexical-mode lr-checkpoint-prefix-snapshot
                 lr-lexical-mode-id
                 lr-checkpoint-resume
                 lr-checkpoint-resume-suffix lr-initial-checkpoint)
        (only-in ./layout
                 make-layout-columns current-layout-columns
                 current-layout-frames)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./significant
                 parser-significant-tokens parser-significant-joined)
        (only-in ./token make-token token-end token-lexeme))
(export current-source-stream-observer parse-source
        parse-source/contextual
        prepare-contextual-parser
        parse-source/contextual/prepared
        parse-source/contextual/prepared/deferred
        parse-source/checkpoints
        parse-source/checkpoints/resume
        parse-tokenized
        parse-tokenized/checkpoint)

(def (contextual-ir-ref ir key)
  (let (row (assq key ir)) (and row (cdr row))))

(def (contextual-canonical value)
  (call-with-output-string (lambda (port) (write value port))))

(def (valid-contextual-product? machine product)
  (and (list? product)
       (equal? (contextual-ir-ref product 'schema)
               "gerbil-parser.contextual-parser-ir.v1")
       (equal? (contextual-ir-ref product 'base-grammar-digest)
               (parser-machine-grammar-digest machine))
       (equal? (contextual-ir-ref product 'parser-ir-digest)
               (sha256-text
                (contextual-canonical (parser-machine-ir machine))))
       (string? (contextual-ir-ref product 'digest))
       (equal?
        (sha256-text
         (contextual-canonical
          (filter (lambda (row) (not (eq? (car row) 'digest)))
                  product)))
        (contextual-ir-ref product 'digest))))

;;; The position table is compiler output: one entry for every LR state. The
;;; scanner receives a finite position id, never the checkpoint or a callback.
(def (validate-contextual-position-table machine scanner-ir table)
  (let* ((positions (contextual-ir-ref scanner-ir 'positions))
        (state-count
         (vector-length
          (cdr
           (assq 'actions
                 (parser-ir-ref (parser-machine-ir machine) 'lr-spec)))))
         (index (make-vector state-count #f)))
    (unless (and (list? table)
                 (= (length table) state-count)
                 (andmap
                  (lambda (row)
                    (and (pair? row) (integer? (car row))
                         (<= 0 (car row))
                         (< (car row) state-count)
                         (symbol? (cdr row))
                         (memq (cdr row) positions)))
                  table))
      (error "invalid contextual LR position table" table))
    (for-each
     (lambda (row)
       (when (vector-ref index (car row))
         (error "invalid contextual LR position table" table))
       (vector-set! index (car row) (cdr row)))
     table)
    index))

;;; Bind immutable generated machine data once; product-owned mutable values
;;; become private indexes, an owned scanner plan and a copied digest.
(defstruct contextual-parser-plan (machine scanner positions digest))

(def (prepare-contextual-parser machine product)
  (unless (valid-contextual-product? machine product)
    (error "contextual parser product does not match parser machine"))
  (let* ((scanner-ir (contextual-ir-ref product 'scanner))
         (positions (validate-contextual-position-table
                     machine scanner-ir (contextual-ir-ref product 'state-positions)))
         (scanner (prepare-contextual-scanner-plan scanner-ir)))
    (make-contextual-parser-plan machine scanner positions
                                 (string-copy (contextual-ir-ref product 'digest)))))

;;; Internal deterministic LR slice. Full contextual admission still requires
;;; declaration-macro binding and GLR branch state. Forks yield rejected artifacts.
(def (parse-source/contextual machine product source)
  (unless (and (string? source)
               (valid-contextual-product? machine product))
    (error "contextual parser product does not match parser machine"))
  (let* ((scanner-ir (contextual-ir-ref product 'scanner))
         (positions (validate-contextual-position-table
                     machine scanner-ir (contextual-ir-ref product 'state-positions)))
         (scanner (prepare-contextual-scanner scanner-ir source)))
    (parse-contextual machine (contextual-ir-ref product 'digest) positions scanner source)))

(def (parse-source/contextual/prepared plan source)
  (unless (and (contextual-parser-plan? plan) (string? source))
    (error "contextual parser requires prepared plan and source"))
  (parse-contextual
   (contextual-parser-plan-machine plan)
   ;; Artifacts expose their digest string; keep publication input-local.
   (string-copy (contextual-parser-plan-digest plan))
   (contextual-parser-plan-positions plan)
   (prepare-contextual-scanner (contextual-parser-plan-scanner plan) source)
   source))

;;; The admitted byte length belongs to this source. Builders validate/count
;;; inside the parse catch and return publication thunks. Serialization errors
;;; therefore remain publication errors, rather than becoming rejected parses.
(def (parse-source/contextual/prepared/deferred plan source source-byte-length success failure)
  (unless (and (contextual-parser-plan? plan) (string? source)
               (integer? source-byte-length) (>= source-byte-length 0)
               (procedure? success) (procedure? failure))
    (error "contextual deferred parser requires admitted source and builders"))
  ((parse-contextual
    (contextual-parser-plan-machine plan)
    (string-copy (contextual-parser-plan-digest plan))
    (contextual-parser-plan-positions plan)
    (prepare-contextual-scanner (contextual-parser-plan-scanner plan) source)
    source success failure source-byte-length)))

(def (parse-contextual machine digest position-table scanner source
                      (success make-success-parse-artifact)
                      (failure failure-artifact) (source-byte-length #f))
  (let* ((state (contextual-scanner-initial-state scanner))
         (tokens-reversed '())
         (trivia? (parser-machine-trivia machine))
         (initial (lr-initial-checkpoint
                   (parser-machine-runtime machine) '())))
    (def (next-input _mode lr-state)
      (let (position (vector-ref position-table lr-state))
        (let-values (((input-token next-state)
                      (contextual-scanner-step scanner state position)))
          (set! state next-state)
          (when input-token
            (set! tokens-reversed (cons input-token tokens-reversed)))
          (if (and input-token (trivia? input-token))
            (next-input _mode lr-state)
            input-token))))
    (def (after-shift _token _states _values _actions _shifts) (void))
    (with-catch
     (lambda (condition)
       (let* ((offset (contextual-scan-state-character-offset state))
              (tokens (reverse tokens-reversed))
              (remaining (substring source offset (string-length source))))
         (failure
          machine digest source
          (if (zero? (string-length remaining))
            tokens
            (append tokens
                    (list (make-token
                           'unknown remaining
                           (contextual-scan-state-byte-offset state)
                           (or source-byte-length (string-utf8-length source))))))
          condition)))
     (lambda ()
       (let-values (((status payload)
                     (lr-checkpoint-drive/contextual
                      initial next-input after-shift)))
         (unless (eq? status 'accepted)
           (error "contextual LR drive requires deterministic acceptance"
                  status))
         (unless (null? (cadr payload))
           (error "contextual LR accepted with trailing tokens"))
         (success
          digest
          source (reverse tokens-reversed) (car payload) trivia?))))))

;; : (-> ParserMachine Exception Diagnostic)
(def current-source-stream-observer (make-parameter #f))

(def (diagnostic machine condition)
  (let* ((recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries)))
         (irritants (error-irritants condition))
         (details
          (and (pair? irritants)
               (let (candidate (car irritants))
                 (and (list? candidate)
                      (assq 'failureKind candidate)
                      candidate)))))
    (append
     (list (cons 'schema +diagnostic-schema+)
           (cons 'code (if row (cadr row) "GERBIL-PARSER-ERROR"))
           (cons 'reasonKind 'parse-rejected)
           (cons 'message (error-message condition)))
     (or details '()))))

;; : (-> ParserMachine Digest String (List Token) Exception ParseArtifact)
(def (failure-artifact machine grammar-digest source tokens condition)
  (make-failure-parse-artifact
   grammar-digest source tokens (diagnostic machine condition)))

;; : (-> ParserMachine Digest String (List Token) ParseArtifact)
(def (parse-tokenized/with machine grammar-digest source tokens
                           parse-significant observability)
  (with-catch
   (lambda (condition)
     (call-with-parser-observed-phase
      observability
      'artifact-materialization
      (lambda ()
        (failure-artifact machine grammar-digest source tokens condition))))
   (lambda ()
     (let* ((significant
             (call-with-parser-observed-phase
              observability
              'significant-token-filter
              (lambda () (parser-significant-tokens machine tokens)))))
       (let-values
         (((root rest)
           (call-with-parser-observed-phase
            observability
            'lr-execution
            (lambda () (parse-significant significant observability)))))
       (unless (null? rest)
         (error "unexpected trailing token" (token-lexeme (car rest))))
       (call-with-parser-observed-phase
        observability
        'artifact-materialization
        (lambda ()
          (make-success-parse-artifact
           grammar-digest source tokens root
           (parser-machine-trivia machine)))))))))

(def (parse-tokenized machine grammar-digest source tokens
                      (observability #f))
  (if (lr-runtime-layout? (parser-machine-runtime machine))
    (parameterize ((current-layout-columns (make-layout-columns source))
                   (current-layout-frames '()))
      (parse-tokenized/with
       machine grammar-digest source tokens
       (lambda (significant observability)
         ((parser-machine-parse machine) significant observability))
       observability))
    (parse-tokenized/with
     machine grammar-digest source tokens
     (lambda (significant observability)
       ((parser-machine-parse machine) significant observability))
     observability)))

;;; Publishes through the same ParseArtifact boundary while the LR phase resumes
;;; an immutable checkpoint bound to this request's significant token stream.
(def (parse-tokenized/checkpoint machine grammar-digest source tokens checkpoint
                                 (observability #f))
  (parse-tokenized/with
   machine grammar-digest source tokens
   (lambda (_significant observability)
     (lr-checkpoint-resume checkpoint observability))
   observability))

;;; A fresh parse keeps one LR execution loop open while the scanner supplies
;;; tokens under the current LR mode. The ordinary checkpoint path below still
;;; owns sparse incremental captures and observed phase boundaries.
(def (parse-source/directed/stream machine grammar-digest source initial
                                   (use-generated? #t))
  (let ((source-length (string-length source))
        (trivia? (parser-machine-trivia machine))
        (character-offset 0)
        (byte-offset 0)
        (pending-character #f)
        (pending-byte #f)
        (tokens-reversed '()))
    (def (next-input mode)
      (if (= character-offset source-length)
        #f
        (let-values
            (((input-token next-character)
              (scan-source-token
               machine source character-offset byte-offset mode)))
          (let ((start-character character-offset)
                (start-byte byte-offset))
            (set! character-offset next-character)
            (set! byte-offset (token-end input-token))
            (if (trivia? input-token)
              (begin
                (set! tokens-reversed
                      (cons input-token tokens-reversed))
                (next-input mode))
              (begin
                (set! pending-character start-character)
                (set! pending-byte start-byte)
                input-token))))))
    (def (after-shift input-token _states _values _actions _shifts)
      (set! tokens-reversed (cons input-token tokens-reversed))
      (set! pending-character #f)
      (set! pending-byte #f))
    (def (publish tokens root rest)
      (unless (null? rest)
        (error "unexpected trailing token" (token-lexeme (car rest))))
      (make-success-parse-artifact
       grammar-digest source tokens root trivia?))
    (let-values (((status payload)
                  (let (direct-drive
                        (and use-generated?
                             (parser-machine-direct-drive machine)))
                    (if direct-drive
                      (direct-drive
                       (parser-machine-runtime machine)
                       next-input after-shift)
                      (lr-checkpoint-drive
                       initial next-input after-shift)))))
      (case status
        ((accepted)
         (unless (and (= character-offset source-length)
                      (not pending-character))
           (error "streamed LR accepted before source end"
                  character-offset source-length))
         (publish (reverse tokens-reversed)
                  (car payload) (cadr payload)))
        ((fallback)
         ;; A generated driver only handles its admitted deterministic path.
         ;; Restart with the original checkpoint and fresh scanner state so
         ;; rejected inputs and GLR forks keep the existing diagnostic owner.
         (parse-source/directed/stream
          machine grammar-digest source initial #f))
        ((fork)
         (let* ((suffix
                 (lex-source-from
                  machine source
                  (or pending-character character-offset)
                  (or pending-byte byte-offset)))
                (tokens (foldl cons suffix tokens-reversed)))
           (let-values (((significant remaining)
                         (parser-significant-joined
                          machine tokens-reversed suffix)))
             (let-values (((root rest)
                           (lr-checkpoint-resume-suffix
                            payload significant remaining)))
               (publish tokens root rest)))))
        (else (error "streamed LR parse did not terminate" status))))))

;;; Deterministic source driver for the sole generated scanner and LR executor.
;;; Each state supplies its interned terminal mode. Trivia advances only the
;;; scanner; significant tokens advance exactly one LR shift. An admitted fork
;;; scans the remaining suffix once and transfers the exact checkpoint to GLR.
(def (parse-source/directed machine grammar-digest source observability capture
                            initial prefix-tokens prefix-modes
                            start-character start-byte reuse-token reuse-fragment)
  (let ((source-length (string-length source))
        (trivia? (parser-machine-trivia machine))
        (source-observer (current-source-stream-observer)))
    (def (publish tokens modes root rest)
      (unless (null? rest)
        (error "unexpected trailing token" (token-lexeme (car rest))))
      (when capture (capture #f tokens modes 0 0))
      (call-with-parser-observed-phase
       observability 'artifact-materialization
       (lambda ()
         (make-success-parse-artifact
          grammar-digest source tokens root trivia?))))
    ;; A resumed checkpoint owns the retained source-token prefix, including
    ;; trivia. Its byte cursor and token cursor must describe the same prefix.
    (when capture (capture initial #f #f (length prefix-tokens) start-byte))
    (let loop ((character-offset start-character)
               (byte-offset start-byte)
               (checkpoint initial)
               (tokens-reversed (reverse prefix-tokens))
               (modes-reversed (reverse prefix-modes))
               (token-count (length prefix-tokens)))
      (if (= character-offset source-length)
        (let-values
            (((root rest)
              (call-with-parser-observed-phase
               observability 'lr-execution
               (lambda () (lr-checkpoint-resume checkpoint observability)))))
          (publish (reverse tokens-reversed)
                   (and capture (reverse modes-reversed))
                   root rest))
        (let (fragment (and reuse-fragment (reuse-fragment character-offset byte-offset checkpoint)))
          (if fragment
            (let* ((next (vector-ref fragment 0)) (segment (vector-ref fragment 1))
                   (segment-modes (vector-ref fragment 2))
                   (next-character (vector-ref fragment 3)) (next-byte (vector-ref fragment 4))
                   (next-count (+ token-count (length segment))))
              (when source-observer (source-observer segment segment-modes #t))
              (when capture (capture next #f #f next-count next-byte #t))
              (loop next-character next-byte next
                    (append (reverse segment) tokens-reversed)
                    (if capture (append (reverse segment-modes) modes-reversed) modes-reversed)
                    next-count))
        (let (mode (lr-checkpoint-lexical-mode checkpoint))
          (let-values
              (((input-token next-character-offset)
                (call-with-parser-observed-phase
                 observability 'lexical-analysis
                 (lambda ()
                   (let (reused
                         (and reuse-token
                              (reuse-token character-offset byte-offset mode)))
                     (if reused
                       (values (car reused) (cdr reused))
                       (scan-source-token
                        machine source character-offset byte-offset mode)))))))
            (when source-observer (source-observer input-token (lr-lexical-mode-id mode) #f))
            (if (call-with-parser-observed-phase
                 observability 'significant-token-filter
                 (lambda () (trivia? input-token)))
              (loop next-character-offset (token-end input-token)
                    checkpoint (cons input-token tokens-reversed)
                    (if capture
                      (cons (lr-lexical-mode-id mode) modes-reversed)
                      modes-reversed)
                    (+ token-count 1))
              (let-values
                  (((status next-checkpoint)
                    (call-with-parser-observed-phase
                     observability 'lr-execution
                     (lambda ()
                       (lr-checkpoint-feed
                        checkpoint input-token observability)))))
                (case status
                  ((checkpoint)
                   (when capture
                     (capture next-checkpoint #f #f
                              (+ token-count 1) (token-end input-token)))
                   (loop next-character-offset (token-end input-token)
                         next-checkpoint
                         (cons input-token tokens-reversed)
                         (if capture
                           (cons (lr-lexical-mode-id mode) modes-reversed)
                           modes-reversed)
                         (+ token-count 1)))
                  ((fork)
                   (when capture
                     (capture #f #f (reverse modes-reversed)
                              token-count byte-offset))
                   (let* ((suffix
                           (call-with-parser-observed-phase
                            observability 'lexical-analysis
                            (lambda ()
                              (lex-source-from
                               machine source
                               character-offset byte-offset))))
                          (tokens
                           (foldl cons suffix tokens-reversed)))
                     (let-values
                         (((significant remaining)
                           (call-with-parser-observed-phase
                            observability 'significant-token-filter
                            (lambda ()
                              (parser-significant-joined
                               machine tokens-reversed suffix)))))
                       (let-values
                           (((root rest)
                             (call-with-parser-observed-phase
                              observability 'lr-execution
                              (lambda ()
                                (lr-checkpoint-resume-suffix
                                 next-checkpoint significant remaining
                                 observability)))))
                         (publish tokens #f root rest)))))
                  (else
                   (error "parser-directed feed did not yield a checkpoint"
                          status)))))))))))))

;; : (-> ParserMachine String ParseArtifact)
(def (parse-source/with-capture machine source observability capture
                                (checkpoint #f) (prefix-tokens '())
                                (prefix-modes '()) (start-character 0)
                                (start-byte 0) (reuse-token #f) (reuse-fragment #f))
  (unless (string? source)
    (error "parse source must be a string" source))
  (let (grammar-digest (parser-machine-grammar-digest machine))
    ;; Lexing is atomic: a lexer either returns its immutable complete token
    ;; list or throws before publication.  Keeping its failure boundary
    ;; separate avoids boxing a mutable token accumulator across the parser's
    ;; exception continuation on every successful request.
    (with-catch
     (lambda (condition)
       ;; The directed success path intentionally does not tokenize rejected
       ;; suffixes. Materialize the ordinary full token stream only on failure
       ;; so diagnostics remain lossless without taxing successful parses.
       (with-catch
        (lambda (_lexical-condition)
          (call-with-parser-observed-phase
           observability 'artifact-materialization
           (lambda ()
             (failure-artifact machine grammar-digest source '() condition))))
        (lambda ()
          (let (tokens
                (call-with-parser-observed-phase
                 observability 'lexical-analysis
                 (lambda () (lex-source machine source))))
            (call-with-parser-observed-phase
             observability 'artifact-materialization
             (lambda ()
               (failure-artifact
                machine grammar-digest source tokens condition)))))))
     (lambda ()
       (let* ((fresh?
               (and (not observability) (not capture)
                    (not checkpoint) (null? prefix-tokens)
                    (zero? start-character) (zero? start-byte)
                    (not reuse-token) (not reuse-fragment)))
              (direct-source
               (and fresh? (parser-machine-direct-source machine)))
              (candidate
               (and direct-source
                    (with-catch
                     (lambda (_condition) #f)
                     (lambda () (direct-source machine source))))))
         (if candidate
           candidate
           (let (initial
                 (or checkpoint
                     (lr-initial-checkpoint
                      (parser-machine-runtime machine) '())))
             (if fresh?
               (parse-source/directed/stream
                machine grammar-digest source initial)
               (parse-source/directed
                machine grammar-digest source observability capture initial
                prefix-tokens prefix-modes start-character start-byte
                reuse-token reuse-fragment)))))))))

(def (parse-source machine source (observability #f))
  (if (lr-runtime-layout? (parser-machine-runtime machine))
    (let (digest (parser-machine-grammar-digest machine))
      (with-catch
       (lambda (condition)
         (failure-artifact machine digest source '() condition))
       (lambda ()
         (parse-tokenized machine digest source
                          (lex-source machine source) observability))))
    (parse-source/with-capture machine source observability #f)))

;; Capture sparse deterministic prefixes during the initial directed parse.
;; A fork keeps the certified prefix; selective GLR owns the uncaptured suffix.
(def (parse-source/checkpoints/resume machine source spacing checkpoint
                                      prefix-tokens prefix-modes
                                      start-character start-byte reuse-token (reuse-fragment #f))
  (unless (and (integer? spacing) (positive? spacing))
    (error "checkpoint spacing must be positive" spacing))
  (if (lr-runtime-layout? (parser-machine-runtime machine))
    ;; Layout branches carry a column-reference stack. The incremental API
    ;; reparses this grammar in full until checkpoints can retain that stack.
    (values (parse-source machine source) #f #f #())
  (let ((snapshots '()) (source-tokens #f) (source-modes #f))
    (let* ((artifact
            (parse-source/with-capture
             machine source #f
             (lambda (checkpoint tokens modes token-count byte-end (force? #f))
               (cond
                (checkpoint
                 (let (shifts
                       (lr-checkpoint-deterministic-shifts checkpoint))
                   (when (or force? (zero? (modulo shifts spacing)))
                     (set! snapshots
                            (cons
                            (vector shifts
                                    (lr-checkpoint-prefix-snapshot checkpoint)
                                    token-count byte-end)
                            snapshots)))))
                (tokens
                 (set! source-tokens tokens)
                 (when modes (set! source-modes modes)))
                (modes (set! source-modes modes))))
             checkpoint prefix-tokens prefix-modes
             start-character start-byte reuse-token reuse-fragment))
           (records
            (if (parse-artifact-success? artifact)
              (list->vector (reverse snapshots))
              #())))
      (values artifact source-tokens source-modes records)))))

(def (parse-source/checkpoints machine source spacing)
  (parse-source/checkpoints/resume
   machine source spacing #f '() '() 0 0 #f))
