;;; -*- Gerbil -*-
;;; Sole semantic-reduction owner shared by generated parser machines.

(import (only-in :std/sugar alet match)
        (only-in ./recognition
                 make-recognition-child make-recognition-fragment
                 make-recognition-node recognition-child-field
                 recognition-child-value
                 recognition-value-end recognition-value-start))
(export recognition-children-field
        recognition-children-alias)

;; children-start
;; : (-> List Fixnum Fixnum)
(def (children-start children default-offset)
  (or (alet (child (and (pair? children) (car children)))
        (recognition-value-start (recognition-child-value child)))
      default-offset))

;; children-end
;; : (-> List Fixnum Fixnum)
(def (children-end children default-offset)
  (if (null? children)
    default-offset
    (recognition-value-end (recognition-child-value (last children)))))

;; recognition-children-field
;; : (-> Symbol List Fixnum List)
(def (recognition-children-field name children default-offset)
  (match children
    ([] '())
    ([child]
     (if (recognition-child-field child)
       (list
        (make-recognition-child
         name
         (make-recognition-fragment
          (children-start children default-offset)
          (children-end children default-offset)
          children)))
       (list (make-recognition-child
              name (recognition-child-value child)))))
    (_
     (list
      (make-recognition-child
       name
       (make-recognition-fragment
        (children-start children default-offset)
        (children-end children default-offset)
        children))))))

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
