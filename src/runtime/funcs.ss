;;; -*- Gerbil -*-
;;; Small immutable sequence algorithms for the LR semantic hot path.

(import (only-in :std/misc/list-builder with-list-builder))
(export association-row-vector->index
        association-row-index-ref
        recognition-sequence-append
        recognition-sequence-concatenate
        recognition-sequence->list)

;;; Convert long association rows into equal?-keyed indexes once, when an
;;; immutable parser machine is prepared. Short rows stay as lists: their
;;; linear scan is cheaper than allocating a hash table, which matters for
;;; small grammars prepared on demand. Values retain the complete source entry
;;; so callers can use both its key and payload without allocating a new pair.
;; : (-> (Vector (List Pair)) (Vector (Or (List Pair) HashTable)))
(def (association-row-vector->index rows)
  (let (count (vector-length rows))
    (def (long-row? index)
      (and (< index count)
           (or (> (length (vector-ref rows index)) 8)
               (long-row? (+ index 1)))))
    (if (not (long-row? 0))
      rows
      (let (indexes (make-vector count))
        (let loop ((index 0))
          (if (= index count)
            indexes
            (let (row (vector-ref rows index))
              (vector-set!
               indexes index
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
              (loop (+ index 1)))))))))

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
  (if (not (recognition-sequence-branch? sequence))
    sequence
    (with-list-builder (collect!)
      (let loop ((pending (list sequence)))
        (unless (null? pending)
          (let (current (car pending))
            (if (recognition-sequence-branch? current)
              (loop
               (cons (recognition-sequence-branch-left current)
                     (cons (recognition-sequence-branch-right current)
                           (cdr pending))))
              (begin
                (for-each collect! current)
                (loop (cdr pending))))))))))
