;;; -*- Gerbil -*-
;;; Private recognition values produced by generated parser machines.

(import (only-in ./token token? token-end token-start)
        (only-in ./event-program event-program-value? event-program-value-start event-program-value-end))
(export relocate-recognition-value
        recognition-relocation? recognition-relocation-value recognition-relocation-delta
        make-recognition-node
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


;;; Position views share the semantic subtree. Collapse repeated moves of the
;;; same view so successive edits do not retain a chain of source snapshots.
(defstruct recognition-relocation (value delta) transparent: #t)
(def (relocate-recognition-value value delta (force? #f))
  (cond
   ((recognition-relocation? value)
    (make-recognition-relocation (recognition-relocation-value value)
                                (+ delta (recognition-relocation-delta value))))
   ((and (zero? delta) (not force?)) value)
   (else (make-recognition-relocation value delta))))

;; : (-> RecognitionValue Nat)
(def (recognition-value-start value)
  (cond
   ((recognition-relocation? value)
    (+ (recognition-relocation-delta value)
       (recognition-value-start (recognition-relocation-value value))))
   ((token? value) (token-start value))
   ((recognition-node? value) (recognition-node-start value))
   ((recognition-fragment? value) (recognition-fragment-start value))
   ((event-program-value? value) (event-program-value-start value))
   (else (error "invalid recognition value" value))))

;; : (-> RecognitionValue Nat)
(def (recognition-value-end value)
  (cond
   ((recognition-relocation? value)
    (+ (recognition-relocation-delta value)
       (recognition-value-end (recognition-relocation-value value))))
   ((token? value) (token-end value))
   ((recognition-node? value) (recognition-node-end value))
   ((recognition-fragment? value) (recognition-fragment-end value))
   ((event-program-value? value) (event-program-value-end value))
   (else (error "invalid recognition value" value))))
