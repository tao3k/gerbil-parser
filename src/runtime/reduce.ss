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
        recognition-children-alias recognition-action-field recognition-action-alias
        make-recognition-field-kernel make-recognition-alias-kernel)

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

(defrule (construct-recognition-field name children default-offset fragment-constructor prepare)
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
                 children default-offset prepare))))))

(defrule (construct-recognition-alias kind children default-offset prepare)
  (list (make-recognition-child #f
          (construct-recognition-bounded (make-recognition-node kind)
            children default-offset prepare))))

(defrule (define-recognition-field reducer prepare)
  (def (reducer name children default-offset
                (fragment-constructor make-recognition-fragment))
    (construct-recognition-field name children default-offset fragment-constructor prepare)))
(defrule (define-recognition-alias reducer prepare)
  (def (reducer kind children default-offset)
    (construct-recognition-alias kind children default-offset prepare)))

;;; Bind grammar-static names outside the request. The canonical constructor
;;; body expands into the kernel, removing its extra semantic wrapper call.
(def (make-recognition-field-kernel name)
  (lambda (children offset constructor)
    (construct-recognition-field name children offset constructor recognition-sequence-for-action)))
(def (make-recognition-alias-kernel kind)
  (lambda (children offset _constructor)
    (construct-recognition-alias kind children offset recognition-sequence-for-action)))

;;; Stage normalization into action constructors. Reading the endpoints from
;;; the shared sequence before flattening avoids a second linear list-tail
;;; scan. Ordinary reducers retain their borrowed child representation.
(defrule (borrow-recognition-children children) children)
(define-recognition-field recognition-children-field borrow-recognition-children)
(define-recognition-alias recognition-children-alias borrow-recognition-children)
(define-recognition-field recognition-action-field recognition-sequence-for-action)
(define-recognition-alias recognition-action-alias recognition-sequence-for-action)
