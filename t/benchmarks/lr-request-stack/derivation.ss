;;; Untimed fresh-request LR derivation diagnostics, shared across languages.
(import (only-in :gerbil-parser/src/compiler/machine parser-machine-ir parser-machine-runtime)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref production-table production-rhs operand-actions)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-parse/prepared current-lr-recognition-observer
                 lr-recognition-fragment? lr-recognition-fragment-production-id
                 lr-recognition-fragment-children lr-recognition-fragment-value
                 lr-recognition-fragment-offset current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/runtime/reduction-plan
                 prepare-lr-reduction-plans reduce-lr-stack-plan)
        (only-in :gerbil-parser/src/runtime/recognition make-recognition-child recognition-child-value)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence-root-child)
        (only-in :std/list/list-builder with-list-builder))
(export profile-lr-request-reductions prepare-lr-semantic-replay
        replay-lr-semantics replay-lr-semantic-control lr-semantic-replay-count)

;;; Captured inputs are diagnostic fixtures, never a second parser. Named
;;; records keep the prepared kernel, borrowed operands and witness distinct.
(defstruct semantic-invocation (plan stack offset expected))
(defstruct lr-semantic-replay (invocations))
(def (lr-semantic-replay-count replay)
  (vector-length (lr-semantic-replay-invocations replay)))

(def (prepare-lr-semantic-replay runtime spec tokens)
  (let* ((plans (prepare-lr-reduction-plans 'recognition
                  (production-table (lr-spec-ref spec 'productions))))
         (root #f))
    (parameterize ((current-lr-event-program-enabled? #f)
                   (current-lr-recognition-observer (lambda (value) (set! root value))))
      (let-values (((_value rest) (lr-parse/prepared runtime tokens)))
        (unless (null? rest) (error "semantic replay left trailing tokens"))))
    (unless (lr-recognition-fragment? root)
      (error "semantic replay requires a deterministic recognition derivation"))
    (let (invocations
          (with-list-builder (emit!)
            (let visit ((pending (list root)))
              (when (pair? pending)
                (let (piece (car pending))
                  (if (lr-recognition-fragment? piece)
                    (let* ((children (lr-recognition-fragment-children piece))
                           (stack (reverse
                                    (map (lambda (child)
                                           (if (lr-recognition-fragment? child)
                                             (lr-recognition-fragment-value child)
                                             (list (make-recognition-child #f child))))
                                         children)))
                           (invocation
                            (make-semantic-invocation
                             (vector-ref plans (lr-recognition-fragment-production-id piece))
                             stack (lr-recognition-fragment-offset piece)
                             (lr-recognition-fragment-value piece))))
                      ;; Validate every kernel, including nullable and decorated
                      ;; operands, before measuring with observers disabled.
                      (unless (equal? (execute-semantic-invocation invocation)
                                      (semantic-invocation-expected invocation))
                        (error "semantic replay differs from the captured reduction"
                               (lr-recognition-fragment-production-id piece)))
                      (emit! invocation)
                      (visit (append children (cdr pending))))
                    (visit (cdr pending))))))))
      (make-lr-semantic-replay (list->vector invocations)))))

(def (execute-semantic-invocation invocation)
  (reduce-lr-stack-plan (semantic-invocation-plan invocation)
                       (semantic-invocation-stack invocation)
                       (semantic-invocation-offset invocation)))

;;; Both traversals retain only their first result (the complete root). The
;;; control measures traversal/result access, not a parser with AST disabled.
;;; Cached input sharing makes replay a component experiment, not additive
;;; attribution of time inside a fresh complete source request.
(defrule (define-semantic-replay name evaluate)
  (def (name replay)
    (let* ((invocations (lr-semantic-replay-invocations replay))
           (root (evaluate (vector-ref invocations 0))))
      (let loop ((index 1))
        (when (fx< index (vector-length invocations))
          (evaluate (vector-ref invocations index))
          (loop (fx+ index 1))))
      (let (child (recognition-sequence-root-child root))
        (unless child
          (error "semantic replay requires one complete recognition root"))
        (recognition-child-value child)))))

(define-semantic-replay replay-lr-semantics execute-semantic-invocation)
(define-semantic-replay replay-lr-semantic-control semantic-invocation-expected)

(def (profile-lr-request-reductions machine tokens)
  (let* ((spec (cdr (assq 'lr-spec (parser-machine-ir machine))))
         (table (production-table (lr-spec-ref spec 'productions)))
         (root #f) (reductions 0) (empty 0) (unary 0) (multiple 0)
         (identity-unary 0) (decorated 0) (longest 0))
    (parameterize ((current-lr-recognition-observer (lambda (value) (set! root value))))
      (let-values (((_value rest) (lr-parse/prepared (parser-machine-runtime machine) tokens)))
        (unless (null? rest) (error "derivation diagnostics left trailing tokens"))))
    (unless root (error "derivation diagnostics require a deterministic fresh request"))
    (let visit ((pending (list (cons root 0))))
      (when (pair? pending)
        (let ((piece (caar pending)) (chain (cdar pending)))
          (if (lr-recognition-fragment? piece)
            (let* ((rhs (production-rhs
                         (vector-ref table (lr-recognition-fragment-production-id piece))))
                   (width (length rhs))
                   (next-chain (if (= width 1) (+ chain 1) 0)))
              (set! reductions (+ reductions 1))
              (case width
                ((0) (set! empty (+ empty 1)))
                ((1) (set! unary (+ unary 1))
                     (when (null? (operand-actions (car rhs)))
                       (set! identity-unary (+ identity-unary 1))))
                (else (set! multiple (+ multiple 1))))
              (set! longest (max longest next-chain))
              (for-each (lambda (operand)
                          (when (pair? (operand-actions operand))
                            (set! decorated (+ decorated 1)))) rhs)
              (visit (append (map (lambda (child) (cons child next-chain))
                                  (lr-recognition-fragment-children piece))
                             (cdr pending))))
            (visit (cdr pending))))))
    (list (cons 'scope 'selected-fresh-derivation)
          (cons 'reductions reductions) (cons 'emptyReductions empty)
          (cons 'unaryReductions unary) (cons 'multiOperandReductions multiple)
          (cons 'identityUnaryReductions identity-unary)
          (cons 'decoratedOperands decorated) (cons 'longestUnaryChain longest))))
