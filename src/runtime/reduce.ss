;;; -*- Gerbil -*-
;;; Sole semantic-reduction owner shared by generated parser machines.

(import (only-in ./recognition
                 make-recognition-child make-recognition-fragment
                 make-recognition-node recognition-child-field
                 recognition-child-value recognition-value-bounds))
(import (only-in ./funcs recognition-sequence-arity recognition-sequence-start
                 recognition-sequence-end recognition-sequence->list
                 recognition-sequence-for-action))
(export recognition-children-field
        recognition-children-alias recognition-action-field recognition-action-alias)

;;; Stage one input-shape decision and one singleton-value classification into
;;; both constructor families. Empty aliases retain their invocation boundary.
(defrule (construct-recognition-bounded (constructor prefix ...) children offset prepare)
  (let-values (((start end)
                (cond
                 ((null? children) (values offset offset))
                 ((and (pair? children) (null? (cdr children)))
                  (let (value (recognition-child-value (car children)))
                    (recognition-value-bounds value)))
                 (else (values (recognition-sequence-start children offset)
                               (recognition-sequence-end children offset))))))
    (constructor prefix ... start end (prepare children))))

(defrule (define-recognition-field reducer prepare)
  (def (reducer name children default-offset
                (fragment-constructor make-recognition-fragment))
    (case (recognition-sequence-arity children)
      ((0) '())
      ((1)
       (let (child (car (recognition-sequence->list children)))
         (if (recognition-child-field child)
           (list (make-recognition-child name
                   (construct-recognition-bounded (fragment-constructor)
                     children default-offset prepare)))
           (list (make-recognition-child name (recognition-child-value child))))))
      (else
       (list (make-recognition-child name
               (construct-recognition-bounded (fragment-constructor)
                 children default-offset prepare)))))))

(defrule (define-recognition-alias name prepare)
  (def (name kind children default-offset)
    (list (make-recognition-child #f
            (construct-recognition-bounded (make-recognition-node kind)
              children default-offset prepare)))))

;;; Stage normalization into action constructors. Reading the endpoints from
;;; the shared sequence before flattening avoids a second linear list-tail
;;; scan. Ordinary reducers retain their borrowed child representation.
(defrule (borrow-recognition-children children) children)
(define-recognition-field recognition-children-field borrow-recognition-children)
(define-recognition-alias recognition-children-alias borrow-recognition-children)
(define-recognition-field recognition-action-field recognition-sequence-for-action)
(define-recognition-alias recognition-action-alias recognition-sequence-for-action)
