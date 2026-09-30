;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-parse parser-machine-runtime
                 parser-machine-trivia parser-machine-direct-drive)
        (only-in ./artifact
                 +diagnostic-schema+ make-failure-parse-artifact
                 make-success-parse-artifact parse-artifact-success?)
        (only-in ./lexer lex-source lex-source-from scan-source-token)
        (only-in ./lr-parser
                 lr-checkpoint-deterministic-shifts lr-checkpoint-drive
                 lr-checkpoint-feed
                 lr-checkpoint-lexical-mode lr-checkpoint-prefix-snapshot
                 lr-lexical-mode-id
                 lr-checkpoint-resume
                 lr-checkpoint-resume-suffix lr-initial-checkpoint)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./significant
                 parser-significant-tokens parser-significant-joined)
        (only-in ./token token-end token-lexeme))
(export parse-source
        parse-source/checkpoints
        parse-source/checkpoints/resume
        parse-tokenized
        parse-tokenized/checkpoint)

;; : (-> ParserMachine Exception Diagnostic)
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
  (parse-tokenized/with
   machine grammar-digest source tokens
   (lambda (significant observability)
     ((parser-machine-parse machine) significant observability))
   observability))

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
                            start-character start-byte reuse-token)
  (let ((source-length (string-length source))
        (trivia? (parser-machine-trivia machine)))
    (def (publish tokens modes root rest)
      (unless (null? rest)
        (error "unexpected trailing token" (token-lexeme (car rest))))
      (when capture (capture #f tokens modes 0 0))
      (call-with-parser-observed-phase
       observability 'artifact-materialization
       (lambda ()
         (make-success-parse-artifact
          grammar-digest source tokens root trivia?))))
    (when capture (capture initial #f #f 0 start-byte))
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
                          status)))))))))))

;; : (-> ParserMachine String ParseArtifact)
(def (parse-source/with-capture machine source observability capture
                                (checkpoint #f) (prefix-tokens '())
                                (prefix-modes '()) (start-character 0)
                                (start-byte 0) (reuse-token #f))
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
       (let (initial
             (or checkpoint
                 (lr-initial-checkpoint (parser-machine-runtime machine) '())))
         (if (and (not observability) (not capture)
                  (not checkpoint) (null? prefix-tokens)
                  (zero? start-character) (zero? start-byte)
                  (not reuse-token))
           (parse-source/directed/stream
            machine grammar-digest source initial)
           (parse-source/directed
            machine grammar-digest source observability capture initial
            prefix-tokens prefix-modes start-character start-byte
            reuse-token)))))))

(def (parse-source machine source (observability #f))
  (parse-source/with-capture machine source observability #f))

;; Capture sparse deterministic prefixes during the initial directed parse.
;; A fork keeps the certified prefix; selective GLR owns the uncaptured suffix.
(def (parse-source/checkpoints/resume machine source spacing checkpoint
                                      prefix-tokens prefix-modes
                                      start-character start-byte reuse-token)
  (unless (and (integer? spacing) (positive? spacing))
    (error "checkpoint spacing must be positive" spacing))
  (let ((snapshots '()) (source-tokens #f) (source-modes #f))
    (let* ((artifact
            (parse-source/with-capture
             machine source #f
             (lambda (checkpoint tokens modes token-count byte-end)
               (cond
                (checkpoint
                 (let (shifts
                       (lr-checkpoint-deterministic-shifts checkpoint))
                   (when (zero? (modulo shifts spacing))
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
             start-character start-byte reuse-token))
           (records
            (if (parse-artifact-success? artifact)
              (list->vector (reverse snapshots))
              #())))
      (values artifact source-tokens source-modes records))))

(def (parse-source/checkpoints machine source spacing)
  (parse-source/checkpoints/resume
   machine source spacing #f '() '() 0 0 #f))
