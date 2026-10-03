#!/usr/bin/env gxi
;;; Source/native scanner scale receipt; compiler and preparation are untimed.
(import
 (only-in :gerbil-parser/src/runtime/contextual-scanner
          prepare-contextual-scanner contextual-scanner-initial-state
          contextual-scanner-step contextual-scan-state-byte-offset)
 (only-in :gerbil-parser/src/compiler/contextual-dispatch compile-contextual-dispatch)
 (only-in :gerbil-parser/src/compiler/contextual-scanner-ir compile-contextual-scanner)
 (only-in :gerbil-parser/src/modules/parser/contextual-objects
          make-contextual-role make-contextual-method make-contextual-scan-rule)
 (only-in :gerbil-parser/src/runtime/token token-lexeme token-kind))
(export main fixture-ir)

(def (catalog prefix size)
  (map (lambda (n) (string->symbol (string-append prefix (number->string n))))
       (iota size)))

(def (fixture-ir axes (literals '()))
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
                mode 'word '(identifier) 0 'keep)
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

(def (main . args)
  (let* ((axes (if (pair? args) (string->number (car args)) 16))
         (samples (if (> (length args) 1) (string->number (cadr args)) 20))
         (words (if (> (length args) 2) (string->number (caddr args)) 128))
         (literal-count (if (> (length args) 3) (string->number (list-ref args 3)) 0)))
    (unless (and (integer? axes) (<= 1 axes 64)
                 (integer? samples) (positive? samples)
                 (integer? words) (positive? words)
                 (integer? literal-count) (<= 0 literal-count 512))
      (error "expected axes in 1..64, positive samples/words, and catalog in 0..512" args))
    (let* ((literals (map (lambda (n) (string-append "word" (number->string n)))
                          (iota literal-count)))
           (source (string-join (if (zero? literal-count) (make-list words "word")
                                 (map (lambda (n) (list-ref '("word0" "word7" "word127" "wording")
                                                            (modulo n 4))) (iota words))) " ")))
      (let-values (((scanner position) (fixture axes source literals)))
        (write (list (cons 'axes axes) (cons 'literals literal-count) (cons 'cells (* 2 axes axes))
                     (cons 'tokens (- (* 2 words) 1)) (cons 'samples samples)))
        (newline) (force-output)
        (let loop ((sample 0))
          (when (< sample (+ samples 3))
            (##gc)
            (let ((started (##current-time-point)) (cpu-started (cpu-time)))
              (let-values (((tokens byte-end) (scan-all scanner position)))
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
                    (newline) (force-output)))))
            (loop (+ sample 1))))
        (display "SCANNER-SCALE-OK\n") (force-output)))))
