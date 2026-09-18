;;; -*- Gerbil -*-
;;; Private recognition values produced by generated parser machines.

(import (only-in ./token token? token-end token-start))
(export make-recognition-node
        recognition-node?
        recognition-node-kind
        recognition-node-start
        recognition-node-end
        recognition-node-children
        make-recognition-fragment
        recognition-fragment?
        recognition-fragment-start
        recognition-fragment-end
        recognition-fragment-children
        make-recognition-child
        recognition-child?
        recognition-child-field
        recognition-child-field-set!
        recognition-child-value
        recognition-value-start
        recognition-value-end)

(defstruct recognition-node (kind start end children) transparent: #t)
(defstruct recognition-fragment (start end children) transparent: #t)
(defstruct recognition-child (field value) transparent: #t)

;; : (-> RecognitionValue Nat)
(def (recognition-value-start value)
  (cond
   ((token? value) (token-start value))
   ((recognition-node? value) (recognition-node-start value))
   ((recognition-fragment? value) (recognition-fragment-start value))
   (else (error "invalid recognition value" value))))

;; : (-> RecognitionValue Nat)
(def (recognition-value-end value)
  (cond
   ((token? value) (token-end value))
   ((recognition-node? value) (recognition-node-end value))
   ((recognition-fragment? value) (recognition-fragment-end value))
   (else (error "invalid recognition value" value))))
