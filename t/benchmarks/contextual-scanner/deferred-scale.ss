#!/usr/bin/env gxi
;;; Deferred queue scale dimension; IR compilation and preparation are untimed.
(import
 (only-in :gerbil-parser/src/modules/parser/contextual-objects
          make-contextual-role make-contextual-method make-contextual-scan-rule)
 (only-in :gerbil-parser/src/compiler/contextual-dispatch compile-contextual-dispatch)
 (only-in :gerbil-parser/src/compiler/contextual-scanner-ir compile-contextual-scanner)
 (only-in :gerbil-parser/src/runtime/contextual-scanner
          prepare-contextual-scanner contextual-scanner-initial-state
          contextual-scanner-step contextual-scan-state-canonical)
 (only-in :gerbil-parser/src/runtime/token token-kind token-lexeme token-start token-end))
(export deferred-scale-ir deferred-scale-source deferred-scale-scan main)

(def (deferred-scale-ir)
  (let* ((forms '(open word space newline body end))
         (role (make-contextual-role
                'deferred-scale
                (map (lambda (form)
                       (make-contextual-method form 'any 'any form form)) forms)))
         (dispatch (compile-contextual-dispatch
                    (list role) '(command body) '(command) forms))
         (rules
          (list
           (make-contextual-scan-rule 'open 'command 'open '(literal "<<") 20
                                     '(expect-marker #f))
           (make-contextual-scan-rule 'word 'command 'word '(identifier) 0
                                     '(enqueue-if-expecting raw))
           (make-contextual-scan-rule 'space 'command 'space
                                     '(horizontal-whitespace+) 0 'keep)
           (make-contextual-scan-rule 'newline 'command 'newline '(newline-one) 0
                                     '(activate-next body))
           (make-contextual-scan-rule 'end 'body 'end '(marker-line) 10
                                     '(finish-marker command body))
           (make-contextual-scan-rule 'body 'body 'body '(body-line) 0 'keep))))
    (compile-contextual-scanner rules dispatch 'command)))

(def (deferred-scale-source count)
  (let (markers (map (lambda (n) (string-append "M" (number->string n)))
                    (iota count)))
    (string-append
     (string-join (map (lambda (marker) (string-append "<<" marker)) markers) " ")
     "\r\n"
     (string-join
            (map (lambda (marker) (string-append "α payload\r\n" marker "\r\n"))
                 markers) ""))))

;;; Returns a complete public token trace plus canonical final checkpoint.
(def (deferred-scale-scan scanner initial step canonical)
  (let loop ((state (initial scanner)) (tokens '()))
    (let-values (((token next) (step scanner state 'command)))
      (if token
        (loop next (cons (list (token-kind token) (token-lexeme token)
                              (token-start token) (token-end token)) tokens))
        (list (reverse tokens) (canonical next))))))

(def (main . args)
  (let* ((count (if (pair? args) (string->number (car args)) 512))
         (samples (if (> (length args) 1) (string->number (cadr args)) 5)))
    (unless (and (integer? count) (positive? count)
                 (integer? samples) (positive? samples))
      (error "expected positive delimiter and sample counts"))
    (let* ((source (deferred-scale-source count))
           (scanner (prepare-contextual-scanner (deferred-scale-ir) source)))
      (let loop ((sample 0))
        (when (< sample samples)
          (##gc)
          (let* ((cpu (cpu-time)) (wall (##current-time-point))
                 (result (deferred-scale-scan
                          scanner contextual-scanner-initial-state
                          contextual-scanner-step contextual-scan-state-canonical))
                 (cpu-ms (* 1000 (- (cpu-time) cpu)))
                 (wall-ms (* 1000 (- (##current-time-point) wall))))
            (unless (and (= (length (car result)) (* 5 count))
                         (equal? (string-join (map cadr (car result)) "") source)
                         (= (cdr (assq 'byteOffset (cadr result)))
                            (u8vector-length (string->utf8 source)))
                         (null? (cdr (assq 'pending (cadr result))))
                         (not (cdr (assq 'active (cadr result)))))
              (error "deferred scale lost tokens, order, or byte coverage"))
            (write (list 'delimiters count 'sample sample 'cpu-ms cpu-ms
                         'elapsed-ms wall-ms))
            (newline) (force-output))
          (loop (+ sample 1))))
      (displayln "DEFERRED-SCALE-OK") (force-output))))
