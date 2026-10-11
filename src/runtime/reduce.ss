;;; -*- Gerbil -*-
;;; Sole semantic-reduction owner shared by generated parser machines.

(import (only-in ./recognition
                 make-recognition-child make-recognition-fragment
                 make-recognition-node recognition-child-field
                 recognition-child-value
                 recognition-value-end recognition-value-start))
(import (only-in ./funcs recognition-sequence-arity recognition-sequence-start recognition-sequence-end recognition-sequence->list))
(export recognition-children-field
        recognition-children-alias)

;; children-start
;; : (-> List Fixnum Fixnum)
(def children-start recognition-sequence-start)
(def children-end recognition-sequence-end)

;; recognition-children-field
;; : (-> Symbol List Fixnum [(-> Fixnum Fixnum List RecognitionFragment)] List)
(def (recognition-children-field name children default-offset
                                 (fragment-constructor make-recognition-fragment))
  (case (recognition-sequence-arity children)
    ((0) '())
    ((1)
     (let (child (car (recognition-sequence->list children)))
       (if (recognition-child-field child)
         (list (make-recognition-child name (fragment-constructor
                 (children-start children default-offset) (children-end children default-offset) children)))
         (list (make-recognition-child name (recognition-child-value child))))))
    (else
     (list (make-recognition-child name (fragment-constructor
             (children-start children default-offset) (children-end children default-offset) children))))))

;; recognition-children-alias
;; : (-> Symbol List Fixnum List)
(def (recognition-children-alias kind children default-offset)
  (list
   (make-recognition-child
    #f
    (make-recognition-node
     kind
     (children-start children default-offset)
     (children-end children default-offset)
     children))))
