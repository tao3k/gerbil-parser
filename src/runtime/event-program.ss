;;; -*- Gerbil -*-
;;; Engine-owned persistent event instructions. No mutable instruction buffer
;;; is exposed. Semantic frames share bodies; token instructions reuse tokens.
(import (only-in ./token token? token-start))
(export event-program-value? event-program-value-kind event-program-value-start
        event-program-value-end event-program-value-code
        event-program-token event-program-append event-program-relocate
        event-program-field event-program-node-value event-program-fragment-value
        event-program-walk)

(defstruct event-code-branch (left right))
(defstruct event-code-view (code delta))
(defstruct event-field-frame (name start end body))
(defstruct event-program-value (kind start end body))

;; A value is itself its node/boundary instruction: no duplicate opcode packet.
(def (event-program-value-code value) value)
(def (event-program-token token) token)
(def (event-program-append left right)
  (cond ((not left) right) ((not right) left)
        (else (make-event-code-branch left right))))
(def (event-program-relocate code delta (moved? #t))
  (cond ((not code) #f) ((not moved?) code)
        ((event-code-view? code)
         (make-event-code-view (event-code-view-code code)
                               (+ delta (event-code-view-delta code))))
        (else (make-event-code-view code delta))))
(def (event-program-field name start end body)
  (make-event-field-frame name start end body))
(def (event-program-node-value kind start end body)
  (make-event-program-value kind start end body))
(def (event-program-fragment-value start end body)
  ;; Flush final trivia before an enclosing field close. Bare tokens do not
  ;; carry this boundary, preserving the original field/trivia order.
  (make-event-program-value #f start end body))

;;; Execute the instruction DAG with an explicit continuation stack. No flat
;;; intermediate tape or translated token objects are allocated. Position views
;;; retain moved provenance even when repeated translations cancel to zero.
(def (event-program-walk visit code)
  (let loop ((code code) (finish? #f) (delta 0) (moved? #f) (pending '()))
    (cond
     ((not code)
      (unless (null? pending)
        (let (frame (car pending))
          (loop (vector-ref frame 0) (vector-ref frame 1)
                (vector-ref frame 2) (vector-ref frame 3) (cdr pending)))))
     (finish?
      (cond
       ((event-program-value? code)
        (visit (if (event-program-value-kind code) 'close-node 'boundary)
               (event-program-value-kind code) (event-program-value-end code) delta moved?))
       ((event-field-frame? code)
        (visit 'close-field (event-field-frame-name code) (event-field-frame-end code) delta moved?))
       (else (error "invalid event continuation")))
      (loop #f #f delta moved? pending))
     ((event-code-view? code)
      (loop (event-code-view-code code) #f (+ delta (event-code-view-delta code)) #t pending))
     ((event-code-branch? code)
      (loop (event-code-branch-left code) #f delta moved?
            (cons (vector (event-code-branch-right code) #f delta moved?) pending)))
     ((event-program-value? code)
      (when (event-program-value-kind code)
        (visit 'open-node (event-program-value-kind code) (event-program-value-start code) delta moved?))
      (loop (event-program-value-body code) #f delta moved?
            (cons (vector code #t delta moved?) pending)))
     ((event-field-frame? code)
      (visit 'open-field (event-field-frame-name code) (event-field-frame-start code) delta moved?)
      (loop (event-field-frame-body code) #f delta moved?
            (cons (vector code #t delta moved?) pending)))
     ((token? code)
      (visit 'token code (token-start code) delta moved?)
      (loop #f #f delta moved? pending))
     (else (error "invalid event instruction")))))
