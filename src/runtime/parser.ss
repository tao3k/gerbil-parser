;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import (only-in ../compiler/parser-ir parser-ir-ref)
        (only-in ../compiler/machine
                 parser-machine-grammar-digest parser-machine-ir
                 parser-machine-parse parser-machine-trivia)
        (only-in ./artifact
                 +diagnostic-schema+ make-failure-parse-artifact
                 make-success-parse-artifact)
        (only-in ./lexer lex-source)
        (only-in ./lr-parser lr-checkpoint-resume)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./significant parser-significant-tokens)
        (only-in ./token token-lexeme))
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
       (call-with-parser-observed-phase
        observability
        'artifact-materialization
        (lambda ()
          (failure-artifact machine grammar-digest source '() condition))))
     (lambda ()
       (parse-tokenized
        machine grammar-digest source
        (call-with-parser-observed-phase
         observability
         'lexical-analysis
         (lambda () (lex-source machine source)))
        observability)))))
