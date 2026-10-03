;;; -*- Gerbil -*-
;;; Engine-owned persistent event instructions. No mutable instruction buffer
;;; is exposed. Semantic frames share bodies; token instructions reuse tokens.
(import (only-in ./token token? token-start token-end))
(export event-program-value? event-program-value-kind event-program-value-start
        event-program-value-end event-program-value-code
        event-program-token event-program-append event-program-relocate
        event-program-field event-program-node-value event-program-fragment-value
        event-program-walk
        event-program-sequence? event-program-sequence-arity
        event-program-sequence-start event-program-sequence-end
        event-program-sequence-field? event-program-sequence-for-each
        event-program-value-body)

(defstruct event-code-branch (left right start end))
(defstruct event-code-view (code delta))
(defstruct event-code-chunk (items start end))
(defstruct event-field-frame (name start end body))
(defstruct event-program-value (kind start end body))

;; A value is itself its node/boundary instruction: no duplicate opcode packet.
(def (event-program-value-code value) value)
(def (event-program-token token) token)
(def +event-chunk-capacity+ 16)
;;; Only a bounded leaf block may be copied. A large prefix remains shared;
;;; append fills its rightmost block with one copied branch, never walks the
;;; existing sequence or rewrites retained blocks.
(def (event-block-width code)
  (cond ((event-code-chunk? code) (vector-length (event-code-chunk-items code)))
        ((event-code-branch? code) (+ +event-chunk-capacity+ 1))
        ((event-code-view? code)
         (if (= (event-program-sequence-arity code) 1) 1 (+ +event-chunk-capacity+ 1)))
        (else 1)))
(def (event-block-join left right)
  (let* ((left-width (event-block-width left)) (right-width (event-block-width right))
         (items (make-vector (+ left-width right-width))))
    (def (copy-block code width offset)
      (if (event-code-chunk? code)
        (let (source (event-code-chunk-items code))
          (let loop ((i 0))
            (when (< i width)
              (vector-set! items (+ offset i) (vector-ref source i))
              (loop (+ i 1)))))
        (vector-set! items offset code)))
    (copy-block left left-width 0)
    (copy-block right right-width left-width)
    (make-event-code-chunk items (event-program-sequence-start left 0)
                                (event-program-sequence-end right 0))))
(def (event-branch left right)
  (make-event-code-branch left right (event-program-sequence-start left 0)
                                   (event-program-sequence-end right 0)))
(def (event-program-append left right)
  (cond
   ((or (not left) (null? left)) right)
   ((or (not right) (null? right)) left)
   ((<= (+ (event-block-width left) (event-block-width right)) +event-chunk-capacity+)
    (event-block-join left right))
   ((and (event-code-branch? left)
         (<= (+ (event-block-width (event-code-branch-right left)) (event-block-width right))
             +event-chunk-capacity+))
    (event-branch (event-code-branch-left left)
                  (event-block-join (event-code-branch-right left) right)))
   (else (event-branch left right))))
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

;;; The same DAG is the LR semantic sequence. Node/boundary values and tokens
;;; are opaque semantic leaves; field frames freeze their label. Bounds on an
;;; append branch are computed once instead of walking its children at actions.
(def (event-program-sequence? value)
  (or (not value) (event-program-value? value) (token? value)
      (event-field-frame? value) (event-code-branch? value) (event-code-view? value)
      (event-code-chunk? value)))
(def (event-program-sequence-arity value)
  (cond ((not value) 0)
        ((event-code-view? value) (event-program-sequence-arity (event-code-view-code value)))
        ((or (event-code-branch? value) (event-code-chunk? value)) 2) (else 1)))
(def (event-program-sequence-bound value default-offset end?)
  (cond
   ((not value) default-offset)
   ((event-code-view? value)
    (+ (event-code-view-delta value)
       (event-program-sequence-bound (event-code-view-code value) default-offset end?)))
   ((event-code-chunk? value)
    ((if end? event-code-chunk-end event-code-chunk-start) value))
   ((event-code-branch? value)
    ((if end? event-code-branch-end event-code-branch-start) value))
   ((event-field-frame? value)
    ((if end? event-field-frame-end event-field-frame-start) value))
   ((event-program-value? value)
    ((if end? event-program-value-end event-program-value-start) value))
   ((token? value) ((if end? token-end token-start) value))
   (else (error "invalid event semantic sequence"))))
(def (event-program-sequence-start value default-offset)
  (event-program-sequence-bound value default-offset #f))
(def (event-program-sequence-end value default-offset)
  (event-program-sequence-bound value default-offset #t))
(def (event-program-sequence-field? value)
  (if (event-code-view? value)
    (event-program-sequence-field? (event-code-view-code value))
    (event-field-frame? value)))

;;; This visitor is the explicit materialization boundary, not a reduction.
;;; Preserve relocation provenance even when its summed offset is zero.
(def (event-program-sequence-for-each visit sequence)
  (let loop ((current sequence) (delta 0) (moved? #f) (pending '()))
    (cond
     ((event-code-view? current)
      (loop (event-code-view-code current) (+ delta (event-code-view-delta current)) #t pending))
     ((event-code-branch? current)
      (loop (event-code-branch-left current) delta moved?
        (cons (vector (event-code-branch-right current) delta moved?) pending)))
     ((event-code-chunk? current)
      (loop (vector-ref (event-code-chunk-items current) 0) delta moved?
        (cons (vector current delta moved? 1) pending)))
     (else
      (when current
        (let unwrap ((value (if (event-field-frame? current) (event-field-frame-body current) current))
                     (delta delta) (moved? moved?))
          (if (event-code-view? value)
            (unwrap (event-code-view-code value) (+ delta (event-code-view-delta value)) #t)
            (visit (and (event-field-frame? current) (event-field-frame-name current)) value delta moved?))))
      (unless (null? pending)
        (let* ((frame (car pending)) (code (vector-ref frame 0)))
          (if (and (= (vector-length frame) 4) (event-code-chunk? code))
            (let* ((items (event-code-chunk-items code)) (i (vector-ref frame 3))
                   (last? (= (+ i 1) (vector-length items))))
              (unless last? (vector-set! frame 3 (+ i 1)))
              (loop (vector-ref items i) (vector-ref frame 1) (vector-ref frame 2)
                    (if last? (cdr pending) pending)))
            (loop code (vector-ref frame 1) (vector-ref frame 2) (cdr pending)))))))))

;;; Execute the instruction DAG with an explicit continuation stack. No flat
;;; intermediate tape or translated token objects are allocated. Position views
;;; retain moved provenance even when repeated translations cancel to zero.
(def (event-program-walk visit code)
  (let loop ((code code) (finish? #f) (delta 0) (moved? #f) (pending '()))
    (cond
     ((not code)
      (unless (null? pending)
        (let* ((frame (car pending)) (next (vector-ref frame 0)))
          (if (and (event-code-chunk? next) (integer? (vector-ref frame 1)))
            (let* ((items (event-code-chunk-items next)) (i (vector-ref frame 1))
                   (last? (= (+ i 1) (vector-length items))))
              (unless last? (vector-set! frame 1 (+ i 1)))
              (loop (vector-ref items i) #f (vector-ref frame 2) (vector-ref frame 3)
                    (if last? (cdr pending) pending)))
            (loop next (vector-ref frame 1) (vector-ref frame 2) (vector-ref frame 3)
                  (cdr pending))))))
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
     ((event-code-chunk? code)
      (loop (vector-ref (event-code-chunk-items code) 0) #f delta moved?
            (cons (vector code 1 delta moved?) pending)))
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
