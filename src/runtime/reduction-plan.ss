;;; -*- Gerbil -*-
;;; Prepared semantic kernels; the LR driver owns stacks and branch admission.
(import (only-in :core/poo-clos/interface .defgeneric .defmethod)
        (only-in :std/vector/vector vector-map/index)
        (only-in ../compiler/lr production-rhs operand-actions)
        (only-in ./recognition make-recognition-fragment)
        (only-in ./reduce recognition-children-field recognition-children-alias)
        (only-in ./funcs recognition-sequence-for-action recognition-sequence-append
                 recognition-sequence-start)
        (only-in ./event-reduce event-children-field event-children-alias)
        (only-in ./event-program event-program-append))
(export prepare-lr-reduction-plans reduce-lr-stack-plan
        reduce-lr-source-plan reduce-lr-event-plan source-operand-offsets)

;;; An empty marked operand is anchored at the next semantic input boundary,
;;; or the production's lookahead boundary when no later value has a start.
;;; Forward executors build this suffix index only upon encountering an empty
;;; marked operand; the reverse stack already has the following children.
(def (source-operand-offsets width values offset start)
  (let (offsets (make-vector width))
    (let fill ((index 0) (values values))
      (when (fx< index width)
        (vector-set! offsets index (start (car values) #f))
        (fill (fx+ index 1) (cdr values))))
    (let resolve ((index (fx- width 1)) (following offset))
      (when (fx>= index 0)
        (let (boundary (or (vector-ref offsets index) following))
          (vector-set! offsets index boundary)
          (resolve (fx- index 1) boundary))))
    offsets))

;;; Canonical operands remain diagnostic data. Preparation stages the finite
;;; backend/action dispatch once; request calls receive their own offset and
;;; fragment constructor. An absent step is the identity operand.
(def (recognition-field-kernel name)
  (lambda (value offset constructor)
    (recognition-children-field name (recognition-sequence-for-action value)
                                offset constructor)))
(def (recognition-alias-kernel name)
  (lambda (value offset _constructor)
    (recognition-children-alias name (recognition-sequence-for-action value) offset)))
(def (event-field-kernel name)
  (lambda (value offset _constructor) (event-children-field name value offset)))
(def (event-alias-kernel name)
  (lambda (value offset _constructor) (event-children-alias name value offset)))

;;; The backend catalog is closed and private. Resolve its generic choices
;;; once at module initialization; grammar preparation binds only rule names.
(.defgeneric select-lr-action-factory (backend kind))
(.defmethod (select-lr-action-factory (backend (eql 'recognition)) (kind (eql 'field)))
  recognition-field-kernel)
(.defmethod (select-lr-action-factory (backend (eql 'recognition)) (kind (eql 'alias)))
  recognition-alias-kernel)
(.defmethod (select-lr-action-factory (backend (eql 'event)) (kind (eql 'field)))
  event-field-kernel)
(.defmethod (select-lr-action-factory (backend (eql 'event)) (kind (eql 'alias)))
  event-alias-kernel)

(def +recognition-action-factories+
  (cons (select-lr-action-factory 'recognition 'field)
        (select-lr-action-factory 'recognition 'alias)))
(def +event-action-factories+
  (cons (select-lr-action-factory 'event 'field)
        (select-lr-action-factory 'event 'alias)))

(def (prepare-operand-actions field alias actions)
  (and (pair? actions)
       (let* ((action (car actions))
              (first ((case (car action) ((field) field) ((alias) alias)) (cadr action)))
              (remaining (prepare-operand-actions field alias (cdr actions))))
         (if remaining
           (lambda (value offset constructor)
             (remaining (first value offset constructor) offset constructor))
           first))))

(def (prepare-reduction-plan field alias production)
  (let* ((operands (production-rhs production))
         (plan (make-vector (length operands))))
    (let loop ((operands operands) (index 0))
      (when (pair? operands)
        (vector-set! plan index (prepare-operand-actions field alias (operand-actions (car operands))))
        (loop (cdr operands) (fx+ index 1))))
    plan))

(def (prepare-lr-reduction-plans backend table)
  (let (factories (case backend
                   ((recognition) +recognition-action-factories+)
                   ((event) +event-action-factories+)
                   (else (error "unknown LR semantic backend" backend))))
    (vector-map/index
     (lambda (_index production)
       (prepare-reduction-plan (car factories) (cdr factories) production)) table)))

;;; The deterministic executor consumes the immutable semantic stack directly.
;;; Each prepared field/alias chain retains declaration order. Prepending each
;;; completed operand restores source order without an intermediate value list.
;;; The canonical projection reducer remains an independent consumer.
(def (reduce-lr-stack-plan actions stack offset)
  (case (vector-length actions)
    ((0) '())
    ((1)
     (let (step (vector-ref actions 0))
       (if step (step (car stack) offset make-recognition-fragment) (car stack))))
    (else
     (let loop ((index (fx- (vector-length actions) 1)) (stack stack)
                (children '()))
       (if (fx>= index 0)
         (let* ((step (vector-ref actions index))
                (input (car stack))
                (value (if step
                         (step input (if (null? input)
                                       (recognition-sequence-start children offset) offset)
                               make-recognition-fragment)
                         input)))
           (loop (fx- index 1) (cdr stack)
                 (recognition-sequence-append value children)))
         children)))))

;;; GLR retains its source-order association and request-local interner.
;;; Event construction retains its own sequential append discipline. Stage
;;; the join and empty value into the same ordered private traversal.
(defrule (define-source-plan-reducer name empty join empty? start)
  (def (name actions values offset constructor)
    (let (width (vector-length actions))
      (case width
        ((0) empty)
        ((1)
         (let (step (vector-ref actions 0))
           (if step (step (car values) offset constructor) (car values))))
        (else
         (let loop ((index 0) (remaining values) (children empty) (offsets #f))
           (if (fx< index width)
             (let* ((step (vector-ref actions index)) (input (car remaining))
                    (anchored? (and step (empty? input)))
                    (offsets (if anchored? (or offsets (source-operand-offsets width values offset start))
                               offsets)))
               (loop (fx+ index 1) (cdr remaining)
                     (join children (if step
                                      (step input (if anchored? (vector-ref offsets index) offset) constructor)
                                      input)) offsets))
             children)))))))

(define-source-plan-reducer reduce-lr-source-plan '() recognition-sequence-append
  null? recognition-sequence-start)
(defrule (empty-event-input? value) (or (not value) (null? value)))
(define-source-plan-reducer reduce-lr-event-plan #f event-program-append
  empty-event-input? recognition-sequence-start)
