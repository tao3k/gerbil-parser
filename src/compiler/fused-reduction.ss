;;; -*- Gerbil -*-
;;; Descriptor-bound fused reductions; no builtin language imports or dispatch.
(import (only-in :clan/poo/object .o .cc .ref .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../language/descriptor language-grammar? language-grammar-ir language-grammar-machine)
        (only-in ./machine parser-machine-grammar-digest parser-machine-ir)
        (only-in ./build-strategy BuildStrategy. build-strategy-common-shape?
                 make-bound-build-strategy declare-build-strategy-provider)
        (only-in ./lr lr-spec-ref production-table production-id production-lhs
                 production-rhs production-action operand-actions operand-actions-valid?))
(export FusedReductionStrategy. FusedReductionStrategyContract
        make-fused-reduction-strategy)

(def (strategy-table candidate)
  (production-table (lr-spec-ref (cdr (assq 'lr-spec (language-grammar-ir (.ref candidate 'descriptor)))) 'productions)))

(def (strategy-name? name)
  (and (symbol? name)
       (not (memq name '(begin import export def quote lambda let let* if case else
                        car cdr cons values error token-start event-program-append
                        make-recognition-fragment recognition-children-field recognition-children-alias
                        event-children-field event-children-alias association-row-index-ref
                        recognition-sequence-for-action recognition-sequence-append recognition-sequence->list)))
       (let (text (symbol->string name))
         (and (> (string-length text) 0) (char-alphabetic? (string-ref text 0))
              (andmap (lambda (ch) (or (char-alphabetic? ch) (char-numeric? ch) (memq ch '(#\- #\_))))
                      (string->list text))))))

;;; Admit the entire action algebra before any form is emitted. Both semantic
;;; backends use the same ordered field/alias declarations and production IDs.
(def (strategy-shape? candidate)
  (with-catch (lambda (_) #f)
    (lambda ()
      (and (build-strategy-common-shape? candidate)
           (eq? (.ref candidate 'provider) +fused-reduction-provider+)
           (andmap (lambda (slot) (.slot? candidate slot)) '(step-name event-name digest-name))
           (let (names (map (lambda (slot) (.ref candidate slot)) '(step-name event-name digest-name)))
             (and (andmap strategy-name? names)
                  (not (eq? (car names) (cadr names)))
                  (not (eq? (car names) (caddr names)))
                  (not (eq? (cadr names) (caddr names)))))
           (let (table (strategy-table candidate))
             (and (> (vector-length table) 0)
                  (let loop ((index 0))
                    (or (= index (vector-length table))
                        (let (production (vector-ref table index))
                          (and (= (production-id production) index)
                               (symbol? (production-lhs production))
                               (memq (production-action production) '(pass concat))
                               (every (lambda (operand)
                                        (operand-actions-valid? (operand-actions operand)))
                                      (production-rhs production))
                               (loop (+ index 1))))))))))))

(define-type (FusedReductionStrategyContract @ PooFlowContract.)
  identity: 'gerbil-parser/fused-reduction-strategy
  .classify: (lambda (candidate context)
               (let (accepted? (strategy-shape? candidate))
                 (poo-flow-classification-evidence
                  'gerbil-parser/fused-reduction-strategy candidate accepted?
                  (if accepted? '() '((expected gerbil-parser/fused-reduction-strategy))) context))))

(def (stack-tail-name prefix index)
  (string->symbol (string-append prefix (number->string index))))

(def (nth-tail name count)
  (let loop ((n count) (value name))
    (if (zero? n) value (loop (fx- n 1) `(cdr ,value)))))

;; Share each stack suffix in generated let* bindings. Repeating an nth-tail
;; expression for every operand makes wide production source quadratic.
(def (stack-value-bindings count)
  (let loop ((remaining count) (tail 'semantic-values) (bindings '()))
    (if (zero? remaining)
      (values (reverse bindings) tail)
      (let* ((index (fx- remaining 1))
             (operand (stack-tail-name "v" index))
             (next (stack-tail-name "tail" index))
             (bound (cons (list operand `(car ,tail)) bindings)))
        (if (zero? index)
          (values (reverse bound) `(cdr ,tail))
          (loop index next (cons (list next `(cdr ,tail)) bound)))))))

(def (operand-expression value operand)
  (foldl
   (lambda (action current)
     (case (car action)
       ((field)
        `(recognition-children-field ',(cadr action)
           (recognition-sequence-for-action ,current) offset
           make-recognition-fragment))
       ((alias)
        `(recognition-children-alias ',(cadr action)
           (recognition-sequence-for-action ,current) offset))
       (else (error "unsupported fused reduction operand action" action))))
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
     (else (error "unsupported fused reduction semantic action" action)))))

(def (step-clause table production-id)
  (let* ((production (vector-ref table production-id))
         (count (length (production-rhs production))))
    (let-values (((bindings remaining-values) (stack-value-bindings count)))
      `((,production-id)
        (let* ((remaining-states ,(nth-tail 'states count))
               ,@bindings
               (remaining-values ,remaining-values)
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
            (values #f #f #f)))))))

(def (step-definition table name)
  `(def (,name production-id states semantic-values rest
                      input-end-offset goto-index)
     (case production-id
       ,@(let loop ((i 0) (acc '()))
           (if (= i (vector-length table)) (reverse acc)
             (loop (fx+ i 1) (cons (step-clause table i) acc))))
       (else (error "unknown generated reduction" production-id)))))
(def (event-step-definition form step-name event-name)
  (cond
   ((eq? form step-name) event-name)
   ((eq? form 'recognition-sequence-append) 'event-program-append)
   ((eq? form 'recognition-children-field) 'event-children-field)
   ((eq? form 'recognition-children-alias) 'event-children-alias)
   ((not (pair? form)) form)
   ((eq? (car form) 'recognition-sequence-for-action)
    (event-step-definition (cadr form) step-name event-name))
   ((eq? (car form) 'quote) form)
   (else (cons (event-step-definition (car form) step-name event-name)
               (event-step-definition (cdr form) step-name event-name)))))

(def (fused-reduction-module strategy)
  (validate FusedReductionStrategyContract strategy)
  (let* ((table (strategy-table strategy)) (name (.ref strategy 'step-name))
         (event-name (.ref strategy 'event-name)) (digest-name (.ref strategy 'digest-name))
         (step (step-definition table name)))
    `(begin
       (import (only-in :gerbil-parser/src/runtime/event-program event-program-append)
               (only-in :gerbil-parser/src/runtime/recognition make-recognition-fragment)
               (only-in :gerbil-parser/src/runtime/reduce recognition-children-field recognition-children-alias)
               (only-in :gerbil-parser/src/runtime/event-reduce event-children-field event-children-alias)
               (only-in :gerbil-parser/src/runtime/funcs association-row-index-ref recognition-sequence->list
                        recognition-sequence-for-action recognition-sequence-append)
               (only-in :gerbil-parser/src/runtime/token token-start))
       (export ,name ,event-name ,digest-name)
       (def ,digest-name ,(.ref strategy 'digest))
       ,step ,(event-step-definition step name event-name))))

(def (emit-fused-reduction-module strategy port)
  ;; Validate and materialize before writing even the first output byte.
  (let (form (fused-reduction-module strategy))
    (display ";;; Generated by the engine FusedReductionStrategy from Parser IR.\n" port)
    (write form port) (newline port)))

(def +fused-reduction-provider+
  (declare-build-strategy-provider 'fused-reductions 'scheme strategy-shape? emit-fused-reduction-module))

(def FusedReductionStrategy.
  (.o (:: self BuildStrategy.)
      provider: +fused-reduction-provider+
      step-name: 'direct-step
      event-name: 'direct-event-step
      digest-name: 'direct-grammar-digest))

(def (make-fused-reduction-strategy descriptor (prototype FusedReductionStrategy.))
  (let (strategy (make-bound-build-strategy descriptor prototype))
    (validate FusedReductionStrategyContract strategy)
    strategy))
