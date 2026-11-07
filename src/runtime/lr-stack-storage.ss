;;; -*- Gerbil -*-
;;; Statically selected LR stack storage; owned buffers never escape a request.
(export make-lr-token-observer lr-token-observer?
        with-lr-stack-storage lr-stack-policy-value lr-stack-current
        lr-stack-present? lr-semantic-stack-present? lr-stack-value
        lr-stack-freeze-states lr-stack-freeze-values
        lr-stack-push-state lr-stack-shift-value lr-stack-push-value
        with-pop-driver-reduction reduce-driver-plan
        lr-stack-shift-observer lr-stack-notify-shift)

(defstruct lr-token-observer (procedure) final: #t constructor: :init!)
(defmethod {:init! lr-token-observer}
  (lambda (self proc)
    (unless (procedure? proc) (error "LR token observer requires a procedure" proc))
    (set! (@ self procedure) proc)))

(def (copy-lr-stack! storage stack)
  (let loop ((rest stack) (index (fx- (length stack) 1)))
    (unless (null? rest)
      (vector-set! storage index (car rest))
      (loop (cdr rest) (fx- index 1)))))
(def (freeze-lr-stack storage end)
  (let loop ((index 0) (result '()))
    (if (fx= index end) result
      (loop (fx+ index 1) (cons (vector-ref storage index) result)))))
(def (grow-lr-stacks states semantic-storage)
  (let* ((width (vector-length states)) (capacity (fx* width 2))
         (next-states (make-vector capacity #f)) (next-values (make-vector capacity #f)))
    (subvector-move! states 0 width next-states 0)
    (subvector-move! semantic-storage 0 width next-values 0)
    (values next-states next-values)))

;;; Only request-owned buffers enter these forms. Initialization establishes
;;; depth < capacity; reduction checks width <= depth, and push grows before
;;; writing. Checkpoint export and borrowed storage retain checked operations.
;;; This is the local array invariant used by std/struct/pqueue, not a module-
;;; wide unchecked declaration or a replacement for runtime table admission.
(defrule (owned-vector-ref storage index)
  (let () (declare (not safe)) (vector-ref storage index)))
(defrule (owned-vector-set! storage index value)
  (let () (declare (not safe)) (vector-set! storage index value)))

(defrules with-lr-stack-storage ()
  ((_ #f states values (state-storage value-storage initial-states initial-values) body ...)
   (let ((state-storage #f) (value-storage #f) (initial-states states) (initial-values values)) body ...))
  ((_ #t states values (state-storage value-storage initial-states initial-values) body ...)
   (let* ((source-states states) (source-values values) (depth (length source-values))
          (capacity (max 16 (fx+ depth 2)))
          (state-storage (make-vector capacity #f)) (value-storage (make-vector capacity #f))
          (initial-states depth) (initial-values #f))
     (unless (= (length source-states) (fx+ depth 1))
       (error "LR state and semantic stack depths differ"))
     (copy-lr-stack! state-storage source-states)
     (copy-lr-stack! value-storage source-values)
     body ...)))
(defrules lr-stack-policy-value ()
  ((_ #f original _owned) original)
  ((_ #t _original owned) owned))
(defrules lr-stack-current ()
  ((_ #f states _storage) (car states))
  ((_ #t top storage) (owned-vector-ref storage top)))
(defrules lr-stack-present? ()
  ((_ #f states) (pair? states))
  ((_ #t top) (fx>= top 0)))
(defrules lr-semantic-stack-present? ()
  ((_ #f values _top) (pair? values))
  ((_ #t _values top) (fx> top 0)))
(defrules lr-stack-value ()
  ((_ #f values _top _storage) (car values))
  ((_ #t _values top storage) (owned-vector-ref storage (fx- top 1))))
(defrules lr-stack-freeze-states ()
  ((_ #f states _storage) states)
  ((_ #t top storage) (freeze-lr-stack storage (fx+ top 1))))
(defrules lr-stack-freeze-values ()
  ((_ #f values _top _storage) values)
  ((_ #t _values top storage) (freeze-lr-stack storage top)))
(defrules lr-stack-push-state ()
  ((_ #f target states _state-storage _value-storage) (cons target states))
  ((_ #t target top state-storage value-storage)
   (let ((next (fx+ top 1)) (next-state target))
     (when (fx>= next (vector-length state-storage))
       (let-values (((next-states next-values) (grow-lr-stacks state-storage value-storage)))
         (set! state-storage next-states) (set! value-storage next-values)))
     (owned-vector-set! state-storage next next-state) next)))
(defrules lr-stack-shift-value ()
  ((_ #f value values _top _storage) (cons value values))
  ((_ #t value _values top storage) (begin (owned-vector-set! storage top value) #f)))
(defrules lr-stack-push-value ()
  ((_ #f value remaining _base _top _storage) (cons value remaining))
  ((_ #t value _remaining base top storage)
   ;; The result overwrites its slot. Clear only the discarded tail; unary
   ;; and epsilon reductions need no bulk call or redundant store.
   (begin
     (when (fx< (fx+ base 1) top)
       (subvector-fill! storage (fx+ base 1) top 0))
     (owned-vector-set! storage base value) #f)))

;;; Prefix validation remains checked. Recognition consumes the original
;;; right-to-left operand order; owned frames need no list-prefix traversal.
(defrules with-pop-driver-reduction ()
  ((_ #f pop states values count events? (source remaining-values remaining-states) body ...)
   (pop states values count events? (source remaining-values remaining-states) body ...))
  ((_ #t _pop states values count _events? (source remaining-values remaining-states) body ...)
   (let ((width count) (source states) (remaining-values values))
     (unless (fx<= width states) (error "LR reduction exceeds parser stack" width))
     (let (remaining-states (fx- states width)) body ...))))
(defrules reduce-driver-plan ()
  ((_ #f reduce _owned-reduce plan values offset events? _storage) (reduce plan values offset events?))
  ((_ #t _reduce owned-reduce plan top offset _events? storage) (owned-reduce plan storage top offset)))

;;; The token-only capability is explicit. Arbitrary five-argument observers
;;; continue receiving immutable list frontiers in the persistent executor.
(defrules lr-stack-shift-observer ()
  ((_ #f observer)
   (if (lr-token-observer? observer)
     (let (proc (lr-token-observer-procedure observer))
       (lambda (token _states _values _actions _shifts) (proc token))) observer))
  ((_ #t observer) (and observer (lr-token-observer-procedure observer))))
(defrules lr-stack-notify-shift ()
  ((_ #f observer token states values actions shifts) (observer token states values actions shifts))
  ((_ #t observer token _states _values _actions _shifts) (observer token)))
