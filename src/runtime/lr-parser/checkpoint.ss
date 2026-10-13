;;; -*- Gerbil -*-
;;; lr-parser: checkpoint owner. Private execution state and named algorithms.

(import (only-in ../reduction-plan
                 reduce-lr-vector-plan)
        (only-in ../../compiler/lr
                 production-lhs)
        (only-in ../recognition
                 make-recognition-child recognition-child-value)
        (only-in ../funcs
                 recognition-sequence-root-child)
        (only-in ../observability
                 call-with-parser-observed-phase)
        (only-in ../token
                 token-end token-start)
        (only-in ../lr-stack-storage
                 with-lr-stack-storage lr-stack-policy-value lr-stack-current lr-stack-present? lr-semantic-stack-present? lr-stack-value lr-stack-freeze-states lr-stack-freeze-values lr-stack-push-state lr-stack-shift-value lr-stack-push-value with-pop-driver-reduction reduce-driver-plan lr-token-observer? lr-stack-shift-observer lr-stack-notify-shift)
        (only-in "./state.ss"
                 lr-runtime-table lr-runtime-reduction-widths lr-runtime-actions lr-runtime-goto-index lr-runtime-lexical-modes lr-runtime-direct-step lr-runtime-semantic-reducer lr-runtime-reduction-plans lr-runtime-action-selector lr-runtime-owned-stack? lr-checkpoint make-lr-checkpoint lr-checkpoint-runtime lr-checkpoint-tokens lr-checkpoint-input-end-offset lr-checkpoint-states lr-checkpoint-semantic-values lr-checkpoint-rest lr-checkpoint-deterministic-actions lr-checkpoint-deterministic-shifts lr-checkpoint-recognition-stack current-lr-recognition-observer make-lr-failure-frontier current-lr-branch-budget)
        (only-in "./recognition.ss"
                 capture-reduction)
        (only-in "./reduction.ss"
                 reduce-value/events reduce-prepared-plan with-pop-runtime-reduction)
        (only-in "./prepare.ss"
                 goto-target)
        (only-in "./batch.ss"
                 lr-parse/prepared/receipt))
(export define-lr-driver
        lr-run-persistent-checkpoint
        lr-run-owned-checkpoint
        lr-run-checkpoint
        lr-checkpoint-advance
        lr-checkpoint-advance-shifts
        lr-checkpoint-feed)

;;; Sole deterministic executor. State remains in local variables on the hot
;;; path; an immutable checkpoint object is allocated only when an explicit
;;; action budget pauses execution. Fork/failure fallback passes the raw local
;;; frontier directly to selective GLR without replay.
;; : (-> LRCheckpoint (OrFalse Nat) PooFlowDebugCallPolicy
;;        (Values Symbol Datum))
(defrule (define-lr-driver policy name)
  (def (name (checkpoint : lr-checkpoint) action-budget observability
                          (stop-at-failure? #f) (shift-target #f)
                          (stop-at-fork? #f) (feed-token #f)
                          (next-input #f) (after-shift #f)
                          (direct-step-override 'installed)
                          (next-input-state? #f) (stop-before-shift? #f))
    (let* ((runtime (lr-checkpoint-runtime checkpoint))
           (selected-observer (lr-stack-shift-observer policy after-shift))
           (direct-step
            (if (eq? direct-step-override 'installed)
              (lr-runtime-direct-step runtime)
              direct-step-override))
           (event-semantics? (lr-stack-policy-value policy
                               (eq? (lr-runtime-semantic-reducer runtime) reduce-value/events) #f))
           (table (lr-runtime-table runtime))
           (widths (lr-runtime-reduction-widths runtime))
           (modes (lr-runtime-lexical-modes runtime))
           (reduction-plans (lr-runtime-reduction-plans runtime))
           (actions-table (lr-runtime-actions runtime))
           (select-action (lr-runtime-action-selector runtime))
           (goto-index (lr-runtime-goto-index runtime))
           (tokens
            (if feed-token
              (cons feed-token (lr-checkpoint-tokens checkpoint))
              (lr-checkpoint-tokens checkpoint)))
           (input-end-offset
            (if feed-token
              (max (lr-checkpoint-input-end-offset checkpoint)
                   (token-end feed-token))
              (lr-checkpoint-input-end-offset checkpoint))))
      (with-lr-stack-storage policy
        (lr-checkpoint-states checkpoint) (lr-checkpoint-semantic-values checkpoint)
        (state-storage value-storage initial-states initial-values)
        (def (fallback states semantic-values rest actions shifts)
          (when (current-lr-recognition-observer)
            ((current-lr-recognition-observer) #f))
          (let-values
              (((root remaining _receipt)
                (call-with-parser-observed-phase
                 observability 'selective-glr-execution
                 (lambda ()
                   (lr-parse/prepared/receipt
                    runtime tokens (current-lr-branch-budget)
                    (lr-stack-freeze-states policy states state-storage)
                    (lr-stack-freeze-values policy semantic-values states value-storage) rest
                    actions shifts)))))
            (values 'accepted (list root remaining))))
        (let loop ((states initial-states)
                   (semantic-values initial-values)
                   (rest (if feed-token
                           (list feed-token)
                           (lr-checkpoint-rest checkpoint)))
                   (actions (lr-checkpoint-deterministic-actions checkpoint))
                   (shifts (lr-checkpoint-deterministic-shifts checkpoint))
                   (remaining-budget (lr-stack-policy-value policy action-budget #f))
                   (recognition-stack (lr-stack-policy-value policy (lr-checkpoint-recognition-stack checkpoint) #f)))
          (if (lr-stack-policy-value policy
                (or (and remaining-budget (zero? remaining-budget))
                    (and shift-target (>= shifts shift-target))) #f)
            (values
             'checkpoint
             (make-lr-checkpoint
              runtime tokens input-end-offset
              (lr-stack-freeze-states policy states state-storage)
              (lr-stack-freeze-values policy semantic-values states value-storage)
              rest actions shifts recognition-stack))
            (let* ((state (lr-stack-current policy states state-storage))
                   (rest
                    (if (and next-input (null? rest))
                      (let (input-token
                            (if next-input-state?
                              (next-input
                               (vector-ref modes state)
                               state)
                              (next-input
                               (vector-ref modes state))))
                        (if input-token
                          (begin
                            (set! tokens (cons input-token tokens))
                            (set! input-end-offset
                                  (max input-end-offset
                                       (token-end input-token)))
                            (list input-token))
                          '()))
                      rest))
                   (action-row
                    (select-action state rest)))
              (if (not action-row)
                (if stop-at-failure?
                  (values
                   'failure
                   (make-lr-failure-frontier
                    (make-lr-checkpoint
                     runtime tokens input-end-offset
                     (lr-stack-freeze-states policy states state-storage)
                     (lr-stack-freeze-values policy semantic-values states value-storage)
                     rest actions shifts recognition-stack)
                    state
                    (map car (vector-ref actions-table state))))
                  (fallback states semantic-values rest actions shifts))
                (let (action (cdr action-row))
                  (case (car action)
                    ((shift)
                     (if (lr-stack-policy-value policy stop-before-shift? #f)
                       (values 'checkpoint
                               (make-lr-checkpoint runtime tokens input-end-offset
                                 (lr-stack-freeze-states policy states state-storage)
                                 (lr-stack-freeze-values policy semantic-values states value-storage)
                                 rest actions shifts recognition-stack))
                     (if (pair? rest)
                       (let ((next-states (lr-stack-push-state policy (cadr action) states state-storage value-storage))
                             (next-values
                              (lr-stack-shift-value policy
                               (if event-semantics? (car rest) (list (make-recognition-child #f (car rest))))
                               semantic-values states value-storage))
                             (next-actions (fx+ actions 1))
                             (next-shifts (fx+ shifts 1)))
                         (when selected-observer
                           (lr-stack-notify-shift policy selected-observer (car rest) next-states next-values
                                               next-actions next-shifts))
                         (loop next-states next-values (cdr rest)
                               next-actions next-shifts
                               (lr-stack-policy-value policy (and remaining-budget (fx- remaining-budget 1)) #f)
                               (and recognition-stack
                                    (cons (car rest) recognition-stack))))
                       (fallback states semantic-values rest actions shifts))))
                    ((reduce)
                     (let (production-id (cadr action))
                       (if direct-step
                         (let-values (((target next-states next-values)
                                       (direct-step
                                        production-id states semantic-values rest
                                        input-end-offset goto-index)))
                           (if target
                             (loop next-states next-values rest
                                   (fx+ actions 1) shifts
                                   (lr-stack-policy-value policy (and remaining-budget (fx- remaining-budget 1)) #f)
                                   (and recognition-stack
                                        (capture-reduction
                                         runtime direct-step production-id states recognition-stack rest
                                         (if (pair? rest) (token-start (car rest)) input-end-offset)
                                         (lr-stack-value policy next-values next-states value-storage) target)))
                             (fallback states semantic-values rest
                                       actions shifts)))
                         (let* ((production (vector-ref table production-id))
                                (count (vector-ref widths production-id)))
                           (with-pop-driver-reduction policy with-pop-runtime-reduction
                             states semantic-values count event-semantics?
                             (source-values remaining-values remaining-states)
                             (let* ((offset
                                     (if (pair? rest) (token-start (car rest))
                                         input-end-offset))
                                    (value
                                     (reduce-driver-plan policy reduce-prepared-plan reduce-lr-vector-plan
                                      (vector-ref reduction-plans production-id)
                                      source-values offset event-semantics? value-storage))
                                    (target
                                     (and (lr-stack-present? policy remaining-states)
                                          (goto-target
                                           goto-index (lr-stack-current policy remaining-states state-storage)
                                           (production-lhs production)))))
                               (if target
                                 (loop
                                  (lr-stack-push-state policy target remaining-states state-storage value-storage)
                                  (lr-stack-push-value policy value remaining-values remaining-states states value-storage)
                                  rest (fx+ actions 1) shifts
                                  (lr-stack-policy-value policy (and remaining-budget (fx- remaining-budget 1)) #f)
                                  (and recognition-stack
                                       (capture-reduction runtime direct-step production-id states
                                         recognition-stack rest offset value target)))
                                 (fallback
                                  states semantic-values rest
                                  actions shifts))))))))
                    ((fork)
                     (if stop-at-fork?
                       (begin
                        (when (current-lr-recognition-observer)
                          ((current-lr-recognition-observer) #f))
                        (values
                        'fork
                        (make-lr-checkpoint
                         runtime tokens input-end-offset
                         (lr-stack-freeze-states policy states state-storage)
                         (lr-stack-freeze-values policy semantic-values states value-storage)
                         rest actions shifts #f)))
                       (fallback states semantic-values rest actions shifts)))
                    ((accept)
                     (let (child
                           (and (lr-semantic-stack-present? policy semantic-values states)
                                (recognition-sequence-root-child
                                 (lr-stack-value policy semantic-values states value-storage))))
                       (if child
                         (begin
                           (when (current-lr-recognition-observer)
                             ((current-lr-recognition-observer)
                              (and recognition-stack (pair? recognition-stack)
                                   (null? (cdr recognition-stack))
                                   (car recognition-stack))))
                           (values
                            'accepted
                            (list (recognition-child-value child) rest)))
                         (fallback
                          states semantic-values rest actions shifts))))
                    (else
                     (fallback states semantic-values rest actions shifts))))))))))))

;;; One transition body has two compile-time storage policies. Immutable
;;; checkpoints and arbitrary stack observers retain persistent frontiers;
;;; unbounded private recognition may own reusable vectors within one call.
(define-lr-driver #f lr-run-persistent-checkpoint)

(define-lr-driver #t lr-run-owned-checkpoint)

(def (lr-run-checkpoint (checkpoint : lr-checkpoint) action-budget observability
                        (stop-at-failure? #f) (shift-target #f)
                        (stop-at-fork? #f) (feed-token #f)
                        (next-input #f) (after-shift #f)
                        (direct-step-override 'installed)
                        (next-input-state? #f) (stop-before-shift? #f))
  (let (runtime (lr-checkpoint-runtime checkpoint))
    (if (and (lr-runtime-owned-stack? runtime) (not action-budget) (not shift-target)
             (not observability) (not stop-before-shift?) (not (current-lr-recognition-observer))
             (not (lr-checkpoint-recognition-stack checkpoint))
             (or (eq? direct-step-override 'installed) (not direct-step-override))
             (or (not after-shift) (lr-token-observer? after-shift)))
      (lr-run-owned-checkpoint checkpoint action-budget observability stop-at-failure? shift-target
        stop-at-fork? feed-token next-input after-shift direct-step-override next-input-state? stop-before-shift?)
      (lr-run-persistent-checkpoint checkpoint action-budget observability stop-at-failure? shift-target
        stop-at-fork? feed-token next-input after-shift direct-step-override next-input-state? stop-before-shift?))))

;;; Advances through the same executor and materializes one checkpoint only at
;;; the requested deterministic action boundary.
(def (lr-checkpoint-advance checkpoint (action-budget 1)
                            (observability #f))
  (unless (and (integer? action-budget) (positive? action-budget))
    (error "LR checkpoint action budget must be positive" action-budget))
  (lr-run-checkpoint checkpoint action-budget observability))

;;; Advances by consumed significant tokens rather than raw actions. This is
;;; the stable boundary used by incremental parsing because reductions do not
;;; consume source input.
(def (lr-checkpoint-advance-shifts checkpoint (shift-budget 1)
                                   (observability #f))
  (unless (and (integer? shift-budget) (positive? shift-budget))
    (error "LR checkpoint shift budget must be positive" shift-budget))
  (lr-run-checkpoint
   checkpoint #f observability #f
   (+ (lr-checkpoint-deterministic-shifts checkpoint) shift-budget)))

;;; Feeds exactly one significant token through reductions and one shift. If
;;; reductions expose a selective-GLR cell first, returns its exact immutable
;;; frontier instead of starting GLR with a truncated one-token suffix.
(def (lr-checkpoint-feed checkpoint input-token (observability #f))
  (lr-run-checkpoint
   checkpoint #f observability #f
   (fx+ (lr-checkpoint-deterministic-shifts checkpoint) 1)
   #t input-token))

