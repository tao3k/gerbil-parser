;;; -*- Gerbil -*-
;;; Engine-owned persistent event bytecode. Blocks never expose mutable storage.
(import (only-in ./token token-start))
(export event-program-value? event-program-value-kind event-program-value-start
        event-program-value-end event-program-value-code
        event-program-token event-program-append event-program-relocate
        event-program-field event-program-node-value event-program-fragment-value
        event-program-walk)

(defstruct event-code-block (code))
(defstruct event-code-branch (left right))
(defstruct event-code-view (code delta))
(defstruct event-program-value (kind start end code))

(def (event-program-token token)
  (make-event-code-block (vector 'token token (token-start token))))
(def (event-program-append left right)
  (cond ((not left) right) ((not right) left)
        (else (make-event-code-branch left right))))
(def (event-program-relocate code delta (moved? #t))
  (cond ((not code) #f)
        ((not moved?) code)
        ((event-code-view? code)
         (make-event-code-view (event-code-view-code code)
                               (+ delta (event-code-view-delta code))))
        (else (make-event-code-view code delta))))
(def (event-program-field name start end body)
  (make-event-code-block
   (vector 'open-field name start 'call body 0 'close-field name end)))
(def (event-program-node-value kind start end body)
  (make-event-program-value kind start end
    (make-event-code-block
     (vector 'open-node kind start 'call body 0 'close-node kind end))))
(def (event-program-fragment-value start end body)
  ;; A fragment flushes its final trivia before the enclosing field closes.
  ;; A token's field close deliberately has no such boundary operation.
  (make-event-program-value #f start end
    (make-event-code-block (vector 'call body 0 'boundary #f end))))

;;; The program is a DAG of private blocks, concatenations and position views.
;;; The walker emits operations in order, without a flat intermediate tape or
;;; translated tokens. A moved view stays moved when two deltas cancel to zero.
(def (event-program-walk visit code)
  (let loop ((code code) (index 0) (delta 0) (moved? #f) (pending '()))
    (cond
     ((not code)
      (unless (null? pending)
        (let (frame (car pending))
          (loop (vector-ref frame 0) (vector-ref frame 1)
                (vector-ref frame 2) (vector-ref frame 3) (cdr pending)))))
     ((event-code-view? code)
      (loop (event-code-view-code code) 0
            (+ delta (event-code-view-delta code)) #t pending))
     ((event-code-branch? code)
      (loop (event-code-branch-left code) 0 delta moved?
            (cons (vector (event-code-branch-right code) 0 delta moved?) pending)))
     ((event-code-block? code)
      (let (instructions (event-code-block-code code))
        (if (= index (vector-length instructions))
          (loop #f 0 delta moved? pending)
          (let* ((operation (vector-ref instructions index))
                 (name (vector-ref instructions (+ index 1)))
                 (offset (vector-ref instructions (+ index 2)))
                 (next (+ index 3)))
            (if (eq? operation 'call)
              (loop name 0 (+ delta offset) moved?
                    (if (= next (vector-length instructions)) pending
                      (cons (vector code next delta moved?) pending)))
              (begin
                (visit operation name offset delta moved?)
                (loop code next delta moved? pending)))))))
     (else (error "invalid event program instruction container")))))
