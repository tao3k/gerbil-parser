;;; -*- Gerbil -*-
;;; Shared indexed-set and source-position algorithms for parser compilation.

(import (only-in :std/list/list-builder with-list-builder)
        (prefix-in :std/struct/queue stdq-))
(export ascii-lower-byte
        compiler-index-set-add
        compiler-index-set-difference
        compiler-index-set-empty?
        compiler-index-set-member?
        compiler-index-set-for-each
        compiler-index-set-reachable
        compiler-index-partition-refine
        compiler-index-set-singleton
        compiler-index-set-union
        compiler-index-set->ordered-values
        compiler-position-index-add!
        compiler-position-index-freeze!
        compiler-position-vector-next
        compiler-u8vector-prefix?)

;; Indexed identities are dense non-negative integers within one compiler
;; domain, so one arbitrary-precision integer is a compact immutable set.
;; : (-> Integer Fixnum Integer)
(def (compiler-index-set-add set index)
  (bitwise-ior set (arithmetic-shift 1 index)))

;; : (-> Fixnum Integer)
(def (compiler-index-set-singleton index)
  (arithmetic-shift 1 index))

;; : (-> Integer Integer Integer)
(def (compiler-index-set-union left right)
  (bitwise-ior left right))

;; : (-> Integer Integer Integer)
(def (compiler-index-set-difference set excluded)
  (bitwise-and set (bitwise-not excluded)))

;; : (-> Integer Boolean)
(def (compiler-index-set-empty? set)
  (zero? set))

;; : (-> Integer Fixnum Boolean)
(def (compiler-index-set-member? set index)
  (not (zero? (bitwise-and set (arithmetic-shift 1 index)))))

;; The public set stays an immutable integer. Wide traversal extracts one
;; positive fixnum-sized field at a time; clearing a field bit never copies
;; the full bigint. Small and extremely sparse sets retain direct traversal.
;; Empty fields count as work: use the direct path when there are fewer set
;; bits than fields spanning the first and last bit. This prevents a long
;; empty gap from turning two callbacks into a scan of every field.
(def compiler-index-field-width (integer-length (##greatest-fixnum)))

;; : (-> Integer Procedure Void)
(def (compiler-index-set-for-each set procedure)
  (def (direct)
    (let loop ((rest set))
      (unless (zero? rest)
        (let (index (first-set-bit rest))
          (procedure index)
          (loop (bitwise-and rest (- rest 1)))))))
  (def (visit field offset)
    (let loop ((rest field))
      (unless (zero? rest)
        (let (index (first-set-bit rest))
          (procedure (+ offset index))
          (loop (bitwise-and rest (- rest 1)))))))
  (if (fixnum? set)
    (let loop ((rest set))
      (unless (zero? rest)
        (let (index (first-set-bit rest))
          (procedure index)
          (loop (bitwise-and rest (- rest 1))))))
    (let* ((limit (integer-length set))
           (width compiler-index-field-width)
           (start (* width (quotient (first-set-bit set) width)))
           (fields (quotient (+ (- limit start) (- width 1)) width)))
      (if (< (bit-count set) fields)
        (direct)
        (let loop ((offset start))
          (when (< offset limit)
            (visit (extract-bit-field width offset set) offset)
            (loop (+ offset width))))))))

;; Reflexive transitive reachability over a vector of immutable index masks.
;; Mark the complete outgoing delta before queuing it: every node is expanded
;; at most once, including self edges, cycles and overlapping initial roots.
;; Only new neighbors are enumerated; membership and union operate once per
;; expanded nonempty row instead of once per outgoing edge.
(def (compiler-index-set-reachable initial edges)
  (let ((reached initial) (queue (stdq-make-Queue)))
    (compiler-index-set-for-each initial (lambda (node) (stdq-enqueue! queue node)))
    (let drain ()
      (unless (stdq-queue-empty? queue)
        (let (neighbors (vector-ref edges (stdq-dequeue! queue)))
          (unless (zero? neighbors)
            (let (delta (compiler-index-set-difference neighbors reached))
              (unless (zero? delta)
                (set! reached (compiler-index-set-union reached delta))
                (compiler-index-set-for-each delta (lambda (node) (stdq-enqueue! queue node)))))))
        (drain)))
    reached))

;; Refine dense indexed partitions with label/set predecessor signatures.
;; Signature equality must be invariant under bijective group renaming.
;; Only splitting a source group can change signature equality at its targets.
;; Global renumbering of unsplit groups is injective and cannot change equality.
;; visit-successors must enumerate every signature dependency, including self
;; edges; observe receives completed group/node/signature counts each round.
;; Input IDs are never mutated. Final IDs follow first-seen node order.
(def (compiler-index-partition-refine initial initial-count signature visit-successors observe)
  (let* ((count (vector-length initial))
         (sizes (make-vector count 0))
         ;; -1: unseen, -2: split, otherwise the first new group identity.
         (first-child (make-vector count -1)))
    (let refine ((ids initial) (group-count initial-count)
                 (dirty (make-vector initial-count #t)))
      (vector-fill! sizes 0)
      (vector-fill! first-child -1)
      (let ((next (make-vector count 0))
            (keys (make-table test: equal?))
            (next-count 0) (examined 0))
        (let count-loop ((node 0))
          (when (< node count)
            (let (group (vector-ref ids node))
              (vector-set! sizes group (+ 1 (vector-ref sizes group))))
            (count-loop (+ node 1))))
        (let node-loop ((node 0))
          (when (< node count)
            (let* ((group (vector-ref ids node))
                   (singleton? (= (vector-ref sizes group) 1))
                   (affected? (vector-ref dirty group))
                   (key (and (not singleton?) affected?
                             (begin (set! examined (+ examined 1))
                                    (cons group (signature node ids)))))
                   (found (cond (singleton? #f)
                                (affected? (table-ref keys key #f))
                                (else (let (first (vector-ref first-child group))
                                        (and (>= first 0) first))))))
              (unless found
                (set! found next-count)
                (set! next-count (+ next-count 1))
                (when (and (not singleton?) affected?)
                  (table-set! keys key found)))
              (vector-set! next node found)
              (let (first (vector-ref first-child group))
                (cond ((= first -1) (vector-set! first-child group found))
                      ((and (>= first 0) (not (= first found)))
                       (vector-set! first-child group -2)))))
            (node-loop (+ node 1))))
        (observe next-count count examined)
        (if (and (> next-count group-count) (< next-count count))
          (let (next-dirty (make-vector next-count #f))
            (let source-loop ((source 0))
              (when (< source count)
                (when (= (vector-ref first-child (vector-ref ids source)) -2)
                  (visit-successors source
                    (lambda (target)
                      (vector-set! next-dirty (vector-ref next target) #t))))
                (source-loop (+ source 1))))
            (refine next next-count next-dirty))
          (values next next-count))))))

;; Materialize in canonical catalog order through the standard library's
;; linear list builder.
;; : (-> Integer Vector List)
(def (compiler-index-set->ordered-values set values)
  (with-list-builder (collect!)
    (compiler-index-set-for-each
     set
     (lambda (index)
       (collect! (vector-ref values index))))))

;; A source-order scan conses offsets into descending lists. Freeze once so
;; repeated forward queries can use binary search without copying any suffix.
(def (compiler-position-index-add! table keys key position)
  (let (positions (hash-get table key))
    (unless positions (set! keys (cons key keys)))
    (hash-put! table key (cons position (or positions '())))
    keys))

(def (compiler-position-index-freeze! table keys)
  (for-each (lambda (key)
              (hash-put! table key (list->vector (hash-get table key))))
            keys))

;; Vectors are descending; return the first source position at or after from.
(def (compiler-position-vector-next positions from)
  (and positions
       (let search ((low 0) (high (vector-length positions)))
         (if (< low high)
           (let (middle (quotient (+ low high) 2))
             (if (>= (vector-ref positions middle) from)
               (search (+ middle 1) high)
               (search low middle)))
           (and (> low 0) (vector-ref positions (- low 1)))))))

(def (ascii-lower-byte value)
  (if (and (<= 65 value) (<= value 90)) (+ value 32) value))

;; Compare a prefix in place so noncandidate source lines allocate no key.
(def (compiler-u8vector-prefix? bytes from until prefix ascii-ci?)
  (let (size (u8vector-length prefix))
    (and (<= (+ from size) until)
         (let compare ((index 0))
           (or (= index size)
               (and (let ((actual (u8vector-ref bytes (+ from index)))
                          (expected (u8vector-ref prefix index)))
                      (= (if ascii-ci? (ascii-lower-byte actual) actual)
                         (if ascii-ci? (ascii-lower-byte expected) expected)))
                    (compare (+ index 1))))))))
