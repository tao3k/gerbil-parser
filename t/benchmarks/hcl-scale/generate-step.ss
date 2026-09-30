;;; -*- Gerbil -*-
;;; Generate HCL fused reductions from the compiled Scheme Parser IR.
;;; The indexed LR action rows remain the state-transition owner.

(import (only-in :gerbil-parser/languages/hcl/v2-24/grammar
                 hcl-v2-24-parser-ir hcl-v2-24-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/compiler/lr
                 lr-spec-ref production-table production-lhs
                 production-rhs production-action operand-actions)
        (only-in :std/misc/ports read-all-as-string))

(def spec (cdr (assq 'lr-spec hcl-v2-24-parser-ir)))
(def table (production-table (lr-spec-ref spec 'productions)))

(def (nth-tail name count)
  (let loop ((n count) (value name))
    (if (zero? n) value (loop (fx- n 1) `(cdr ,value)))))

(def (operand-expression value operand)
  (foldl
   (lambda (action current)
     (case (car action)
       ((field)
        `(recognition-children-field ',(cadr action)
           (recognition-sequence->list ,current) offset
           make-recognition-fragment))
       ((alias)
        `(recognition-children-alias ',(cadr action)
           (recognition-sequence->list ,current) offset))
       (else (error "unsupported HCL operand action" action))))
   value (operand-actions operand)))

(def (semantic-expression production)
  (let* ((rhs (production-rhs production))
         (count (length rhs))
         (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (= count 1))
      (operand-expression 'v0 (car rhs)))
     ((memq action '(pass concat))
      (let loop ((operands rhs) (i 0) (combined ''()))
        (if (null? operands)
          combined
          (loop (cdr operands) (fx+ i 1)
                `(recognition-sequence-append
                  ,combined
                  ,(operand-expression
                    (string->symbol
                     (string-append "v" (number->string i)))
                    (car operands)))))))
     (else (error "unsupported HCL semantic action" action)))))

(def (step-clause production-id)
  (let* ((production (vector-ref table production-id))
         (count (length (production-rhs production)))
         (bindings
          (let loop ((i 0) (acc '()))
            (if (= i count)
              (reverse acc)
              (loop (fx+ i 1)
                    (cons
                     (list
                      (string->symbol
                       (string-append "v" (number->string i)))
                      `(car ,(nth-tail 'semantic-values (fx- count i 1))))
                     acc)))))
         (remaining-states (nth-tail 'states count))
         (remaining-values (nth-tail 'semantic-values count)))
    `((,production-id)
      (let* ((remaining-states ,remaining-states)
             (remaining-values ,remaining-values)
             ,@bindings
             (offset (if (pair? rest) (token-start (car rest))
                       input-end-offset))
             (value ,(semantic-expression production))
             (entry (and (pair? remaining-states)
                         (association-row-index-ref
                          goto-index (car remaining-states)
                          ',(production-lhs production))))
             (target (and entry (cdr entry))))
        (if target
          (values target (cons target remaining-states)
                  (cons value remaining-values))
          (values #f #f #f))))))

(def (module-expression)
  `(begin
     (import (only-in :gerbil-parser/src/runtime/recognition
                      make-recognition-fragment)
             (only-in :gerbil-parser/src/runtime/reduce
                      recognition-children-field recognition-children-alias)
             (only-in :gerbil-parser/src/runtime/funcs
                      association-row-index-ref
                      recognition-sequence->list recognition-sequence-append)
             (only-in :gerbil-parser/src/runtime/token token-start))
     (export direct-step direct-grammar-digest)
     (def direct-grammar-digest
       ,(parser-machine-grammar-digest hcl-v2-24-parser))
     (def (direct-step production-id states semantic-values rest
                       input-end-offset goto-index)
       (case production-id
         ,@(let loop ((i 0) (acc '()))
             (if (= i (vector-length table))
               (reverse acc)
               (loop (fx+ i 1) (cons (step-clause i) acc))))
         (else (error "unknown generated HCL step" production-id))))))

(def (emit-module port)
  (display ";;; Generated from HCL v2.24 Parser IR; regenerate with generate-step.ss.\n"
           port)
  (write (module-expression) port)
  (newline port))

(def (main . args)
  (cond
   ((and (= (length args) 2) (equal? (car args) "module"))
    (call-with-output-file (cadr args) emit-module))
   ((and (= (length args) 2) (equal? (car args) "check"))
    (let ((expected (call-with-output-string emit-module))
          (actual (call-with-input-file (cadr args) read-all-as-string)))
      (unless (string=? expected actual)
        (error "generated HCL fused step is stale" (cadr args)))
      (displayln "GENERATED-HCL-STEP-OK")))
   (else (error "expected module|check generated source path" args))))
(export main)
