;;; -*- Gerbil -*-
;;; lr-parser: reduction owner. Private execution state and named algorithms.

(import (only-in ../reduction-plan
                 reduce-lr-stack-plan reduce-lr-event-plan source-operand-offsets)
        (only-in ../event-program
                 event-program-append)
        (only-in ../event-reduce
                 event-children-field event-children-alias)
        (only-in ../../compiler/lr
                 operand-actions production-action layout-end-action? production-rhs)
        (only-in ../reduce
                 recognition-children-alias recognition-children-field)
        (only-in ../funcs
                 recognition-sequence-append recognition-sequence-start recognition-sequence-for-action))
(export apply-operand-action
        define-operand-action-fold
        apply-operand-actions
        reduce-operands
        reduce-value
        apply-operand-action/events
        apply-operand-actions/events
        reduce-operands/events
        reduce-value/events
        reduce-prepared-plan
        with-pop-reduction
        reduction-stack-tail
        reduction-source-values
        with-pop-runtime-reduction)

;; apply-operand-action
;; : (-> List List Fixnum List)
(def (apply-operand-action action children default-offset
                           fragment-constructor)
  (let (materialized (recognition-sequence-for-action children))
  (case (car action)
    ((field)
     (recognition-children-field
      (cadr action) materialized default-offset fragment-constructor))
    ((alias)
     (recognition-children-alias (cadr action) materialized default-offset))
    (else (error "unknown LR operand action" action)))))

;;; An admitted chain is an ordered fold, independent of its semantic backend.
;;; Stage the backend operation into a closed tail loop, as std/iter stages
;;; iterator dispatch. No request-time callback captures offset or constructor.
(defrule (define-operand-action-fold fold-name action-step)
  (def (fold-name value actions offset constructor)
    (if (pair? actions)
      (fold-name (action-step (car actions) value offset constructor)
                 (cdr actions) offset constructor)
      value)))

(define-operand-action-fold apply-operand-actions apply-operand-action)

;;; Keep the two-list reduction loop closed: offset and constructor travel
;;; as explicit parameters, so no callback captures them for each reduction.
;;; Like foldl2, consume the lists together in source order.
(def (reduce-operands operands values offset constructor children offsets index)
  (if (and (pair? operands) (pair? values))
    (reduce-operands
     (cdr operands) (cdr values) offset constructor
     (recognition-sequence-append
      children (apply-operand-actions
                (car values) (operand-actions (car operands))
                (if offsets (vector-ref offsets index) offset) constructor))
     offsets (fx+ index 1))
    children))

;; reduce-value
;; : (-> List List Fixnum List)
(def (reduce-value production source-values default-offset
                   fragment-constructor)
  (let* ((rhs (production-rhs production))
         (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (pair? rhs) (null? (cdr rhs)))
      (apply-operand-actions
       (car source-values) (operand-actions (car rhs))
       default-offset fragment-constructor))
     ((or (eq? action 'concat) (eq? action 'pass)
          (layout-end-action? action))
      (reduce-operands rhs source-values default-offset fragment-constructor '()
        (and (any null? source-values)
             (source-operand-offsets (length rhs) source-values default-offset recognition-sequence-start)) 0))
     (else (error "unknown LR semantic action" action)))))

(defrule (apply-operand-action/events action-expr value offset ignored-constructor)
  (let (action action-expr)
    (case (car action)
      ((field) (event-children-field (cadr action) value offset))
      ((alias) (event-children-alias (cadr action) value offset))
      (else (error "unknown event semantic action" action)))))

(define-operand-action-fold apply-operand-actions/events apply-operand-action/events)

(def (reduce-operands/events operands values offset children offsets index)
  (if (and (pair? operands) (pair? values))
    (reduce-operands/events
     (cdr operands) (cdr values) offset
     (event-program-append
      children (apply-operand-actions/events
                (car values) (operand-actions (car operands))
                (if offsets (vector-ref offsets index) offset) #f))
     offsets (fx+ index 1))
    children))

(def (reduce-value/events production source-values offset ignored-constructor)
  (let ((rhs (production-rhs production)) (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (pair? rhs) (null? (cdr rhs)))
      (apply-operand-actions/events (car source-values)
        (operand-actions (car rhs)) offset #f))
     ((memq action '(pass concat))
      (reduce-operands/events rhs source-values offset #f
        (and (any (lambda (value) (or (not value) (null? value))) source-values)
             (source-operand-offsets (length rhs) source-values offset recognition-sequence-start)) 0))
     (else (error "unsupported event LR production" action)))))

;;; Keep prepared-plan access outside the LR continuation's generated body.
(def (reduce-prepared-plan plan values offset events?)
  (if events?
    (reduce-lr-event-plan plan values offset #f)
    (reduce-lr-stack-plan plan values offset)))

;;; Bind the popped immutable stack prefixes directly into the reduction body.
;;; Both private reducers stop at the RHS width, so unary reductions borrow the
;;; first semantic cell. Wider reductions build a source-order value list.
;;; The named loop passes the suffixes as locals instead of returning an
;;; intermediate multiple-value container to an immediately unpacking caller.
(defrule (with-pop-reduction states semantic-values count
                             (source-values remaining-values remaining-states)
                             body ...)
  (let* ((initial-states states) (initial-values semantic-values)
         (width count) (unary? (eqv? width 1)))
    (let pop ((remaining width) (remaining-states initial-states)
              (remaining-values initial-values)
              (source-values (if unary? initial-values '())))
      (if (zero? remaining)
        (begin body ...)
        (if (and (pair? remaining-states) (pair? remaining-values))
          (pop (fx- remaining 1) (cdr remaining-states) (cdr remaining-values)
               (if unary? source-values
                 (cons (car remaining-values) source-values)))
          (error "LR reduction exceeds parser stack" width))))))

;;; Recognition borrows the stack. Event programs retain source-order operands
;;; for their amortized sequential append builder; right-associated joins are
;;; semantically equivalent but need not have the same construction cost.
(def (reduction-stack-tail stack width)
  (let loop ((stack stack) (remaining width))
    (cond ((zero? remaining) stack)
          ((pair? stack) (loop (cdr stack) (fx- remaining 1)))
          (else (error "LR reduction exceeds parser stack" width)))))

(def (reduction-source-values stack width)
  ;; The caller already validated the prefix. Unary event reductions borrow
  ;; one value; wider event reductions retain their sequential append order.
  (if (eqv? width 1) stack
    (let loop ((stack stack) (remaining width) (values '()))
      (if (zero? remaining) values
        (loop (cdr stack) (fx- remaining 1) (cons (car stack) values))))))

(defrule (with-pop-runtime-reduction states semantic-values count events?
                                    (source-values remaining-values remaining-states)
                                    body ...)
  (let* ((width count) (stack semantic-values)
         (remaining-states (reduction-stack-tail states width))
         (remaining-values (reduction-stack-tail stack width))
         (source-values (if events? (reduction-source-values stack width) stack)))
    body ...))

