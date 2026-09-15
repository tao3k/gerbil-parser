;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-parse parser-machine-runtime
                 parser-machine-trivia)
        (only-in ./artifact
                 +diagnostic-schema+ make-failure-parse-artifact
                 make-success-parse-artifact)
        (only-in ./lexer lex-source lex-source-from scan-source-token)
        (only-in ./lr-parser
                 lr-checkpoint-feed lr-checkpoint-lexical-mode
                 lr-checkpoint-resume
                 lr-checkpoint-resume-suffix lr-initial-checkpoint)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token token-end token-lexeme))
(export parse-source
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

;;; Deterministic source driver for the sole generated scanner and LR executor.
;;; Each state supplies its interned terminal mode. Trivia advances only the
;;; scanner; significant tokens advance exactly one LR shift. An admitted fork
;;; scans the remaining suffix once and transfers the exact checkpoint to GLR.
(def (parse-source/directed machine grammar-digest source observability)
  (let ((source-length (string-length source))
        (trivia? (parser-machine-trivia machine)))
    (def (publish tokens root rest)
      (unless (null? rest)
        (error "unexpected trailing token" (token-lexeme (car rest))))
      (call-with-parser-observed-phase
       observability 'artifact-materialization
       (lambda ()
         (make-success-parse-artifact
          grammar-digest source tokens root trivia?))))
    (let loop ((character-offset 0)
               (byte-offset 0)
               (checkpoint
                (lr-initial-checkpoint
                 (parser-machine-runtime machine) '()))
               (tokens-reversed '()))
      (if (= character-offset source-length)
        (let-values
            (((root rest)
              (call-with-parser-observed-phase
               observability 'lr-execution
               (lambda () (lr-checkpoint-resume checkpoint observability)))))
          (publish (reverse tokens-reversed) root rest))
        (let (mode (lr-checkpoint-lexical-mode checkpoint))
          (let-values
              (((input-token next-character-offset)
                (call-with-parser-observed-phase
                 observability 'lexical-analysis
                 (lambda ()
                   (scan-source-token
                    machine source character-offset byte-offset mode)))))
            (if (call-with-parser-observed-phase
                 observability 'significant-token-filter
                 (lambda () (trivia? input-token)))
              (loop next-character-offset (token-end input-token)
                    checkpoint (cons input-token tokens-reversed))
              (let-values
                  (((status next-checkpoint)
                    (call-with-parser-observed-phase
                     observability 'lr-execution
                     (lambda ()
                       (lr-checkpoint-feed
                        checkpoint input-token observability)))))
                (case status
                  ((checkpoint)
                   (loop next-character-offset (token-end input-token)
                         next-checkpoint
                         (cons input-token tokens-reversed)))
                  ((fork)
                   (let* ((suffix
                           (call-with-parser-observed-phase
                            observability 'lexical-analysis
                            (lambda ()
                              (lex-source-from
                               machine source
                               character-offset byte-offset))))
                          (tokens
                           (append (reverse tokens-reversed) suffix)))
                     (let-values
                         (((significant remaining)
                           (call-with-parser-observed-phase
                            observability 'significant-token-filter
                            (lambda ()
                              (values
                               (parser-significant-tokens machine tokens)
                               (parser-significant-tokens machine suffix))))))
                       (let-values
                           (((root rest)
                             (call-with-parser-observed-phase
                              observability 'lr-execution
                              (lambda ()
                                (lr-checkpoint-resume-suffix
                                 next-checkpoint significant remaining
                                 observability)))))
                         (publish tokens root rest)))))
                  (else
                   (error "parser-directed feed did not yield a checkpoint"
                          status)))))))))))

;; : (-> ParserMachine String ParseArtifact)
(def (parse-source machine source
                   (observability #f))
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
       (parse-source/directed
        machine grammar-digest source observability)))))
