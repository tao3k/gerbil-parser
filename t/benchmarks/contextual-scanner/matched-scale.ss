#!/usr/bin/env gxi
;;; Source/native scanner scale receipt; optional preparation is timed, IR compilation is not.
(import
 (only-in :gerbil-parser/src/runtime/contextual-scanner
          prepare-contextual-scanner prepare-contextual-scanner-plan contextual-scanner-initial-state
          contextual-scanner-step contextual-scan-state-byte-offset)
 (only-in :gerbil-parser/src/compiler/contextual-dispatch compile-contextual-dispatch)
 (only-in :gerbil-parser/src/compiler/contextual-scanner-ir compile-contextual-scanner)
 (only-in :gerbil-parser/src/modules/parser/contextual-objects
          make-contextual-role make-contextual-method make-contextual-scan-rule)
 (only-in :gerbil-parser/src/runtime/token token-lexeme token-kind))
(import (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))
(export main fixture-ir benchmark-matcher-preparation)

(def (catalog prefix size)
  (map (lambda (n) (string->symbol (string-append prefix (number->string n))))
       (iota size)))

(def (fixture-ir axes (literals '()) (word-matcher '(identifier)))
  (let* ((modes (catalog "m" axes))
         (positions (catalog "p" axes))
         (role (make-contextual-role
                'scanner-scale
                (list (make-contextual-method 'word 'any 'any 'word 'word)
                      (make-contextual-method 'space 'any 'any 'space 'space))))
         (dispatch
          (compile-contextual-dispatch (list role) modes positions '(word space)))
         (rules
          (apply append
           (map
            (lambda (mode)
              (append (if (null? literals) '()
                        (list (make-contextual-scan-rule
                               (string->symbol (string-append (symbol->string mode) "-catalog"))
                               mode 'word (list 'literals literals) 10 'keep)))
              (list
               (make-contextual-scan-rule
                (string->symbol (string-append (symbol->string mode) "-word"))
                mode 'word word-matcher 0 'keep)
               (make-contextual-scan-rule
                (string->symbol (string-append (symbol->string mode) "-space"))
                mode 'space '(horizontal-whitespace+) 0 'keep))))
            modes)))
         (ir (compile-contextual-scanner rules dispatch (last modes))))
    (values ir (last positions))))

(def (fixture axes source (literals '()))
  (let-values (((ir position) (fixture-ir axes literals)))
    (values (prepare-contextual-scanner ir source) position)))

(def (scan-all scanner position)
  (let loop ((state (contextual-scanner-initial-state scanner)) (tokens '()))
    (let-values (((token next) (contextual-scanner-step scanner state position)))
      (if token (loop next (cons token tokens))
        (values (reverse tokens) (contextual-scan-state-byte-offset next))))))

;;; Compare revisions through this same caller; no baseline executor ships here.
;;; IR construction and product checks remain outside the preparation interval.
(def (benchmark-matcher-preparation (samples 11) (iterations 3))
  (unless (and (exact-integer? samples) (positive? samples)
               (exact-integer? iterations) (positive? iterations))
    (error "matcher preparation requires positive exact sample/iteration counts"))
  (let* ((literals (map (lambda (n) (string-append "word" (number->string n))) (iota 128)))
         (stops (append '(" " ";")
                        (map (lambda (n) (string-append "stop" (number->string n))) (iota 128))))
         (region (list 'region-word
                       (list stops '((#\" #t ("${"))) '(("${" #\{ #\} 1)) #f)))
         (plain-words '("word0" "word7" "word127" "wording"))
         (region-words '("${α}" "\"β\"" "word0" "wording")))
    (for-each
     (match <>
       ([name axes catalog matcher words]
        (let* ((expected-words (apply append (make-list 32 words)))
               (source (string-join expected-words " "))
               (source-bytes (u8vector-length (string->utf8 source))))
          (let-values (((ir position) (fixture-ir axes catalog matcher)))
            (let sample ((index 0))
              (when (< index samples)
                (##gc)
                (let* ((before (##process-statistics))
                       (plan (let repeat ((left iterations) (plan #f))
                               (if (zero? left) plan
                                 (repeat (- left 1) (prepare-contextual-scanner-plan ir)))))
                       (after (##process-statistics))
                       (delta (lambda (slot) (- (f64vector-ref after slot) (f64vector-ref before slot)))))
                  (let-values (((tokens byte-end) (scan-all (prepare-contextual-scanner plan source) position)))
                    (unless (and (= byte-end source-bytes)
                                 (= (length tokens) (- (* 2 (length expected-words)) 1))
                                 (equal? (map token-lexeme (filter (lambda (token) (eq? (token-kind token) 'word)) tokens))
                                         expected-words)
                                 (equal? (string-join (map token-lexeme tokens) "") source))
                      (error "prepared matcher product differs from declared words" name)))
                  (write (list 'matcher-preparation name index 'iterations iterations
                               'cpu-ms-per-call (/ (* 1000 (+ (delta 0) (delta 1))) iterations)
                               'allocated-bytes-per-call
                               (let (bytes (admit-parser-allocation (delta 7) (delta 6)))
                                 (and bytes (/ bytes iterations)))
                               'gc-count (delta 6)))
                  (newline) (force-output))
                (sample (+ index 1))))))))
     (list (list 'identifier-one 1 '() '(identifier) plain-words)
           (list 'identifier-sixteen 16 '() '(identifier) plain-words)
           (list 'literal-sixteen 16 literals '(identifier) plain-words)
           (list 'region-sixteen 16 literals region region-words)
           (list 'guarded-region-sixteen 16 literals
                 (list 'unless-prefix '("<<<") '() region) region-words))))
  (displayln "MATCHER-PREPARATION-OK") (force-output))

(def (main . args)
  (let* ((axes (if (pair? args) (string->number (car args)) 16))
         (samples (if (> (length args) 1) (string->number (cadr args)) 20))
         (words (if (> (length args) 2) (string->number (caddr args)) 128))
         (literal-count (if (> (length args) 3) (string->number (list-ref args 3)) 0))
         (phase (if (> (length args) 4) (list-ref args 4) "scan"))
         (requests (if (> (length args) 5) (string->number (list-ref args 5)) 1)))
    (unless (and (integer? axes) (<= 1 axes 64)
                 (integer? samples) (positive? samples)
                 (integer? words) (positive? words)
                 (integer? literal-count) (<= 0 literal-count 512)
                 (integer? requests) (positive? requests)
                 (member phase '("scan" "prepare-scan" "plan-batch")))
      (error "expected axes in 1..64, positive samples/words/requests, catalog in 0..512, phase scan, prepare-scan or plan-batch" args))
    (let* ((literals (map (lambda (n) (string-append "word" (number->string n)))
                          (iota literal-count)))
           (source (string-join (if (zero? literal-count) (make-list words "word")
                                 (map (lambda (n) (list-ref '("word0" "word7" "word127" "wording")
                                                            (modulo n 4))) (iota words))) " ")))
      (let-values (((ir position) (fixture-ir axes literals)))
       (let (scanner (prepare-contextual-scanner ir source))
        (write (list (cons 'axes axes) (cons 'phase phase) (cons 'literals literal-count) (cons 'cells (* 2 axes axes))
                     (cons 'tokens (- (* 2 words) 1)) (cons 'samples samples) (cons 'requests requests)))
        (newline) (force-output)
        (let loop ((sample 0))
          (when (< sample (+ samples 3))
            (##gc)
            (let ((started (##current-time-point)) (cpu-started (cpu-time)))
              (let* ((plan (and (string=? phase "plan-batch") (prepare-contextual-scanner-plan ir)))
                     (result
                      (let batch ((left requests) (result #f))
                        (if (zero? left) result
                          (let-values (((tokens byte-end)
                                        (scan-all (cond (plan (prepare-contextual-scanner plan source))
                                                        ((string=? phase "prepare-scan") (prepare-contextual-scanner ir source))
                                                        (else scanner)) position)))
                            (batch (- left 1) (cons tokens byte-end)))))))
               (let ((tokens (car result)) (byte-end (cdr result)))
                (let ((cpu-ms (* 1000.0 (- (cpu-time) cpu-started)))
                      (wall-ms (* 1000.0 (- (##current-time-point) started))))
                  (unless (and (= (length tokens) (- (* 2 words) 1))
                               (= byte-end (string-length source))
                               (equal? (string-join (map token-lexeme tokens) "") source)
                               (= (length (filter (lambda (token) (eq? (token-kind token) 'word)) tokens)) words))
                    (error "scanner scale receipt failed semantic or byte coverage"))
                  (when (>= sample 3)
                    (write (list (cons 'sample (- sample 3))
                                 (cons 'cpu-ms cpu-ms) (cons 'elapsed-ms wall-ms)))
                    (newline) (force-output))))))
            (loop (+ sample 1))))
        (display "SCANNER-SCALE-OK\n") (force-output))))))
