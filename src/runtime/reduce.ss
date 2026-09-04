;;; -*- Gerbil -*-
;;; Sole semantic-reduction owner shared by generated parser machines.

(import ./recognition)
(export recognition-children-field
        recognition-children-alias)

(def (children-start children default-offset)
  (if (pair? children)
    (recognition-value-start (recognition-child-value (car children)))
    default-offset))

(def (children-end children default-offset)
  (if (null? children)
    default-offset
    (let loop ((rest children))
      (if (null? (cdr rest))
        (recognition-value-end (recognition-child-value (car rest)))
        (loop (cdr rest))))))

(def (recognition-children-field name children default-offset)
  (cond
   ((null? children) '())
   ((and (null? (cdr children))
         (not (recognition-child-field (car children))))
    (recognition-child-field-set! (car children) name)
    children)
   (else
    (list
     (make-recognition-child
      name
      (make-recognition-fragment
       (children-start children default-offset)
       (children-end children default-offset)
       children))))))

(def (recognition-children-alias kind children default-offset)
  (list
   (make-recognition-child
    #f
    (make-recognition-node
     kind
     (children-start children default-offset)
     (children-end children default-offset)
     children))))
