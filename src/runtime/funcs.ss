;;; -*- Gerbil -*-
;;; Small immutable sequence algorithms for the LR semantic hot path.

(import (only-in ./event-program
                 event-program-sequence? event-program-sequence-arity
                 event-program-sequence-start event-program-sequence-end
                 event-program-sequence-for-each event-program-relocate)
        (only-in ./recognition
                 make-recognition-child recognition-child-field recognition-child-value
                 relocate-recognition-value recognition-value-start recognition-value-end)
        (only-in :std/list/list-builder with-list-builder)
        (only-in :std/vector/vector vector-map/index))
(export association-row-vector->index
        association-row-index-ref
        make-value-interner
        value-interner-intern
        value-interner-created-count
        value-interner-hit-count
        vector-intern-map
        recognition-sequence-append
        recognition-sequence-concatenate
        recognition-sequence->list
        recognition-sequence-for-action
        recognition-sequence-relocate
        current-recognition-sequence-fusion-enabled?
        recognition-sequence-arity recognition-sequence-start recognition-sequence-end
        recognition-sequence-for-each)

;;; Request-local hash-consing over immutable structural keys. Gerbil's
;;; standard equal?-table owns lookup; callers provide the canonical value
;;; constructor, which is invoked only on a miss.
(defstruct value-interner-state (table counters missing) transparent: #t)

(def (make-value-interner)
  (make-value-interner-state
   (make-table test: equal?) (vector 0 0) (cons #f #f)))

(def (value-interner-intern interner key constructor)
  (let* ((table (value-interner-state-table interner))
         (missing (value-interner-state-missing interner))
         (found (table-ref table key missing))
         (counters (value-interner-state-counters interner)))
    (if (eq? found missing)
      (let (value (constructor))
        (table-set! table key value)
        (vector-set! counters 0 (fx+ (vector-ref counters 0) 1))
        value)
      (begin
        (vector-set! counters 1 (fx+ (vector-ref counters 1) 1))
        found))))

(def (value-interner-created-count interner)
  (vector-ref (value-interner-state-counters interner) 0))

(def (value-interner-hit-count interner)
  (vector-ref (value-interner-state-counters interner) 1))

;;; Maps an immutable vector while hash-consing equal projected keys. This is
;;; the shared O(n) construction primitive for state catalogs such as lexical
;;; modes: standard vector traversal owns iteration and the standard equal?
;;; table owns canonicalization. The second result is the compact canonical
;;; catalog in stable id order.
;; : (-> Vector Procedure Procedure (Values Vector Vector))
(def (vector-intern-map source key-of constructor)
  (let* ((interner (make-value-interner))
         (canonical-reversed '())
         (mapped
          (vector-map/index
           (lambda (_index value)
             (let (key (key-of value))
               (value-interner-intern
                interner key
                (lambda ()
                  (let (canonical
                        (constructor
                         key (value-interner-created-count interner)))
                    (set! canonical-reversed
                      (cons canonical canonical-reversed))
                    canonical)))))
           source)))
    (values mapped (list->vector (reverse canonical-reversed)))))

;;; Convert long association rows into equal?-keyed indexes once, when an
;;; immutable parser machine is prepared. Short rows stay as lists: their
;;; linear scan is cheaper than allocating a hash table, which matters for
;;; small grammars prepared on demand. Values retain the complete source entry
;;; so callers can use both its key and payload without allocating a new pair.
;; : (-> (Vector (List Pair)) (Vector (Or (List Pair) HashTable)))
(def (association-row-vector->index rows)
  (if (not (vector-any (lambda (row) (> (length row) 8)) rows))
    rows
    (vector-map/index
     (lambda (_index row)
       (if (<= (length row) 8)
         row
         (let (table (make-hash-table size: (length row)))
           (for-each
            (lambda (entry)
              ;; Match `assoc`: the first equal key is authoritative.
              (unless (hash-get table (car entry))
                (hash-put! table (car entry) entry)))
            row)
           table)))
     rows)))

;; : (-> (Vector (Or (List Pair) HashTable)) Fixnum Value (OrFalse Pair))
(def (association-row-index-ref indexes state key)
  (let (index (vector-ref indexes state))
    (if (list? index)
      (assoc key index)
      (hash-get index key))))

;; A branch shares both input sequences. Proper lists remain leaf chunks, so
;; singleton shifts and already-materialized field/alias results allocate no
;; wrapper. This turns left-recursive repetition from repeated list copying
;; into constant-time concatenation.
(def current-recognition-sequence-fusion-enabled? (make-parameter #f))
(defstruct recognition-sequence-branch (left right) transparent: #t)

(defstruct recognition-sequence-position-view (value delta) transparent: #t)
(def (recognition-sequence-relocate value delta)
  (cond
   ((event-program-sequence? value) (event-program-relocate value delta))
   ((null? value) '())
   ((recognition-sequence-position-view? value)
    (make-recognition-sequence-position-view
     (recognition-sequence-position-view-value value)
     (+ delta (recognition-sequence-position-view-delta value))))
   (else (make-recognition-sequence-position-view value delta))))

;; : (-> RecognitionSequence RecognitionSequence RecognitionSequence)
(def (recognition-sequence-append left right)
  (cond
   ((null? left) right)
   ((null? right) left)
   (else (make-recognition-sequence-branch left right))))

;; : (-> (List RecognitionSequence) RecognitionSequence)
(def (recognition-sequence-concatenate sequences)
  (foldl (lambda (sequence combined)
           (recognition-sequence-append combined sequence))
         '()
         sequences))

;;; Cardinality is capped at two: semantic fields distinguish empty,
;;; singleton and multiple children, not the total length of a sequence.
(def (recognition-sequence-arity sequence)
  (cond
   ((recognition-sequence-position-view? sequence)
    (recognition-sequence-arity (recognition-sequence-position-view-value sequence)))
   ;; Append removes empty operands and relocate preserves empty as empty.
   ;; Therefore a branch always contains at least two semantic children.
   ((recognition-sequence-branch? sequence) 2)
   ((and (not (pair? sequence)) (event-program-sequence? sequence))
    (event-program-sequence-arity sequence))
   ((null? sequence) 0) ((null? (cdr sequence)) 1) (else 2)))
(def (recognition-sequence-bound sequence default-offset end?)
  (let loop ((current sequence) (delta 0))
    (cond
     ((and (not (pair? current)) (event-program-sequence? current))
      (+ delta ((if end? event-program-sequence-end event-program-sequence-start) current default-offset)))
     ((null? current) default-offset)
     ((recognition-sequence-position-view? current)
      (loop (recognition-sequence-position-view-value current)
            (+ delta (recognition-sequence-position-view-delta current))))
     ((recognition-sequence-branch? current)
      (loop (if end? (recognition-sequence-branch-right current)
                      (recognition-sequence-branch-left current)) delta))
     (else (+ delta ((if end? recognition-value-end recognition-value-start)
                    (recognition-child-value (if end? (last current) (car current)))))))))
(def (recognition-sequence-start sequence default-offset)
  (recognition-sequence-bound sequence default-offset #f))
(def (recognition-sequence-end sequence default-offset)
  (recognition-sequence-bound sequence default-offset #t))

;;; Consume original child order and accumulated position views without
;;; building a translated child list. The visitor owns publication/materialization.
(def (recognition-sequence-for-each visit sequence)
  (if (list? sequence)
    (for-each (lambda (child) (visit child 0 #f)) sequence)
    (if (event-program-sequence? sequence)
      (event-program-sequence-for-each
       (lambda (field value delta moved?)
         (visit (make-recognition-child field value) delta moved?)) sequence)
    (let loop ((current sequence) (delta 0) (moved? #f) (pending '()))
      (cond
       ((recognition-sequence-position-view? current)
        (loop (recognition-sequence-position-view-value current)
              (+ delta (recognition-sequence-position-view-delta current)) #t pending))
       ((recognition-sequence-branch? current)
        ;; Descend left in registers; only the deferred right side needs a frame.
        (loop (recognition-sequence-branch-left current) delta moved?
              (cons (vector (recognition-sequence-branch-right current) delta moved?) pending)))
       (else
        (for-each (lambda (child) (visit child delta moved?)) current)
        (unless (null? pending)
          (let (frame (car pending))
            (loop (vector-ref frame 0) (vector-ref frame 1) (vector-ref frame 2)
                  (cdr pending))))))))))
(def (recognition-sequence->list sequence)
  (if (list? sequence) sequence
    (with-list-builder (collect!)
      (recognition-sequence-for-each
       (lambda (child delta moved?)
         (collect! (if (not moved?) child
                     (make-recognition-child (recognition-child-field child)
                       (relocate-recognition-value (recognition-child-value child) delta #t)))))
       sequence))))

;;; The interpreter and generated operand actions share this one boundary.
(def (recognition-sequence-for-action children)
  (if (current-recognition-sequence-fusion-enabled?) children
    (recognition-sequence->list children)))
