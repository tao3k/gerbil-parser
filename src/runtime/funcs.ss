;;; -*- Gerbil -*-
;;; Small immutable sequence algorithms for the LR semantic hot path.

(import (only-in ./recognition
                 make-recognition-child recognition-child-field recognition-child-value
                 relocate-recognition-value)
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
        recognition-sequence-relocate)

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
(defstruct recognition-sequence-branch (left right) transparent: #t)

(defstruct recognition-sequence-position-view (value delta) transparent: #t)
(def (recognition-sequence-relocate value delta)
  (cond
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

;; Iterative depth-first traversal avoids both quadratic append and stack
;; growth when a repeat production has built a deeply left-associated rope.
;; : (-> RecognitionSequence List)
(def (recognition-sequence->list sequence)
  (if (list? sequence)
    sequence
    (with-list-builder (collect!)
      (let loop ((pending (list (vector sequence 0 #f))))
        (unless (null? pending)
          (let* ((frame (car pending)) (current (vector-ref frame 0)) (delta (vector-ref frame 1))
                 (moved? (vector-ref frame 2)))
            (cond
             ((recognition-sequence-position-view? current)
              (loop (cons
                     (vector (recognition-sequence-position-view-value current)
                             (+ delta (recognition-sequence-position-view-delta current)) #t)
                     (cdr pending))))
             ((recognition-sequence-branch? current)
              (loop (cons (vector (recognition-sequence-branch-left current) delta moved?)
                          (cons (vector (recognition-sequence-branch-right current) delta moved?)
                                (cdr pending)))))
             (else
              (for-each
               (lambda (child)
                 (collect!
                  (if (not moved?) child
                    (make-recognition-child
                     (recognition-child-field child)
                     (relocate-recognition-value (recognition-child-value child) delta #t)))))
               current)
              (loop (cdr pending))))))))))
