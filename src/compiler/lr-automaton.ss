;;; -*- Gerbil -*-
;;; Packed LR items, transition indexes, and deterministic LR(0) interning.

(import (only-in ./funcs compiler-index-set-add
                 compiler-index-set-difference compiler-index-set-for-each
                 compiler-index-set-union)
        (only-in :std/struct/queue
                 dequeue! enqueue! make-Queue queue-empty?)
        (only-in ./lr
                 base-symbol nonterminal-name nonterminal-symbol?
                 production-id production-index-by-lhs production-rhs))
(export build-lr0-automaton
        core-item-after-dot core-item-tail-after-next item-after-dot
        item-complete? item-lookahead item-production-id item<?
        make-core-item make-core-symbol-catalog make-item-layout
        core-item-count core-item-production-id core-item-dot
        materialize-transitions
        transition-index transition-target)

;;; Dense dotted positions preserve production/dot ordering without padding
;;; every rule to the widest RHS. The inverse catalog stages decoding once;
;;; all construction routes share this private representation.
(defstruct lr-item-layout (terminal-count offsets owners) final: #t)

(def (make-item-layout table terminal-values)
  (let* ((count (vector-length table)) (offsets (make-vector (+ count 1))))
    (let index ((id 0) (offset 0))
      (vector-set! offsets id offset)
      (if (= id count)
        (let (owners (make-vector offset))
          (let production-loop ((id 0))
            (when (< id count)
              (let fill ((item (vector-ref offsets id))
                         (end (vector-ref offsets (+ id 1))))
                (when (< item end)
                  (vector-set! owners item id)
                  (fill (+ item 1) end)))
              (production-loop (+ id 1))))
          (make-lr-item-layout (vector-length terminal-values) offsets owners))
        (index (+ id 1)
               (+ offset 1 (length (production-rhs (vector-ref table id)))))))))

(def (item-lookahead item layout) (modulo item (lr-item-layout-terminal-count layout)))
(def (item-body item layout) (quotient item (lr-item-layout-terminal-count layout)))
(def (item-production-id item layout)
  (core-item-production-id (item-body item layout) layout))
(def (item-dot item layout) (core-item-dot (item-body item layout) layout))

(def (item-after-dot item layout table)
  (let* ((production (vector-ref table (item-production-id item layout)))
         (rhs (production-rhs production))
         (dot (item-dot item layout)))
    (and (< dot (length rhs)) (base-symbol (list-ref rhs dot)))))

(def (item-complete? item layout table)
  (not (item-after-dot item layout table)))

(def (item<? left right) (< left right))

(def (materialize-transitions state-transitions state-count)
  (foldr (lambda (state found)
           (foldr (lambda (row tail)
                    (cons (list state (car row) (cdr row)) tail))
                  found
                  (vector-ref state-transitions state)))
         '()
         (iota state-count)))

(def (transition-index transitions)
  (let (index (make-table test: equal?))
    (for-each
     (lambda (row)
       (table-set! index (list (car row) (cadr row)) (caddr row)))
     transitions)
    index))

(def (transition-target transitions state symbol)
  (table-ref transitions (list state symbol) #f))

(def (make-core-item production-id dot layout)
  (+ dot (vector-ref (lr-item-layout-offsets layout) production-id)))
(def (core-item-count layout) (vector-length (lr-item-layout-owners layout)))
(def (core-item-production-id item layout)
  (vector-ref (lr-item-layout-owners layout) item))
(def (core-item-dot item layout)
  (- item (vector-ref (lr-item-layout-offsets layout) (core-item-production-id item layout))))

;; Pre-index every valid dotted production position once. LR closure,
;; propagation, and action construction then resolve after-dot symbols with one
;; vector-ref instead of repeated quotient/modulo/length/list-ref traversal.
;; : (-> Vector lr-item-layout Vector)
(def (make-core-symbol-catalog table layout)
  (let ((catalog (make-vector (core-item-count layout) #f)))
    (let production-loop ((production-id 0))
      (when (< production-id (vector-length table))
        (let symbol-loop
            ((rest (production-rhs (vector-ref table production-id)))
             (dot 0))
          (unless (null? rest)
            (vector-set! catalog
                         (make-core-item production-id dot layout)
                         (base-symbol (car rest)))
            (symbol-loop (cdr rest) (+ dot 1))))
        (production-loop (+ production-id 1))))
    catalog))

(def (core-item-after-dot item layout table)
  (let* ((production (vector-ref table
                                 (core-item-production-id item layout)))
         (rhs (production-rhs production))
         (dot (core-item-dot item layout)))
    (and (< dot (length rhs)) (base-symbol (list-ref rhs dot)))))

(def (core-item-tail-after-next item layout table)
  (let* ((production (vector-ref table
                                 (core-item-production-id item layout)))
         (rhs (production-rhs production)))
    (list-tail rhs (+ (core-item-dot item layout) 1))))

;; LR(0) prediction is a grammar-only reachability relation. Solve it once
;; over dense production-id masks, then apply it directly to every goto kernel.
(def (make-core-prediction-catalog core-symbols productions-by-lhs layout)
  (let ((name-index (make-table test: eq?))
        (names '())
        (node-count 0)
        (item-nodes (make-vector (vector-length core-symbols) #f)))
    (let index-loop ((item 0))
      (when (< item (vector-length core-symbols))
        (let (symbol (vector-ref core-symbols item))
          (when (and symbol (nonterminal-symbol? symbol))
            (let* ((name (nonterminal-name symbol))
                   (known (table-ref name-index name #f))
                   (node (or known node-count)))
              (unless known
                (table-set! name-index name node)
                (set! names (cons (cons name node) names))
                (set! node-count (+ node-count 1)))
              (vector-set! item-nodes item node))))
        (index-loop (+ item 1))))
    (let ((masks (make-vector node-count 0))
          (pending (make-vector node-count 0))
          (parents (make-vector node-count '()))
          (queued (make-vector node-count #f))
          (queue (make-Queue)))
      (for-each
       (lambda (entry)
         (let ((node (cdr entry)) (own 0) (children 0))
           (for-each
            (lambda (production)
              (let* ((id (production-id production))
                     (child (vector-ref item-nodes
                                        (make-core-item id 0 layout))))
                (set! own (compiler-index-set-add own id))
                (when child
                  (set! children (compiler-index-set-add children child)))))
            (table-ref productions-by-lhs (car entry) '()))
           (vector-set! masks node own)
           (vector-set! pending node own)
           (unless (zero? own)
             (vector-set! queued node #t)
             (enqueue! queue node))
           (compiler-index-set-for-each
            children
            (lambda (child)
              (vector-set! parents child
                           (cons node (vector-ref parents child)))))))
       names)
      ;; Reverse dependency edges carry only new production bits. Recursive
      ;; grammar components converge before any LR state is constructed.
      (let propagate ()
        (unless (queue-empty? queue)
          (let* ((node (dequeue! queue))
                 (delta (vector-ref pending node)))
            (vector-set! pending node 0)
            (vector-set! queued node #f)
            (for-each
             (lambda (parent)
               (let (novel
                     (compiler-index-set-difference
                      delta (vector-ref masks parent)))
                 (unless (zero? novel)
                   (vector-set! masks parent
                                (compiler-index-set-union
                                 (vector-ref masks parent) novel))
                   (vector-set! pending parent
                                (compiler-index-set-union
                                 (vector-ref pending parent) novel))
                   (unless (vector-ref queued parent)
                     (vector-set! queued parent #t)
                     (enqueue! queue parent)))))
             (vector-ref parents node)))
          (propagate)))
      (let (catalog (make-vector (vector-length core-symbols) 0))
        (let materialize ((item 0))
          (when (< item (vector-length item-nodes))
            (let (node (vector-ref item-nodes item))
              (when node
                (vector-set! catalog item (vector-ref masks node))))
            (materialize (+ item 1))))
        catalog))))

;; Symbol IDs are local to one construction, including template scratch.
(def (make-lr0-symbol-catalog core-symbols)
  (let ((index (make-table test: equal?))
        (ids (make-vector (vector-length core-symbols) #f))
        (count 0))
    (let loop ((item 0))
      (when (< item (vector-length core-symbols))
        (let (symbol (vector-ref core-symbols item))
          (when symbol
            (let (id (table-ref index symbol #f))
              (unless id
                (set! id count)
                (set! count (+ count 1))
                (table-set! index symbol id))
              (vector-set! ids item id))))
        (loop (+ item 1))))
    (let (symbols (make-vector count #f))
      (hash-for-each (lambda (symbol id) (vector-set! symbols id symbol)) index)
      (values symbols ids))))

;; Each distinct prediction mask owns one immutable template: sorted dot-zero
;; items, goto groups ordered by their earliest item, and a sparse group index.
;; A row is #(first-item symbol-id advanced-kernel). Templates share across
;; states; their group masks never enter mutable scratch by reference mutation.
(def (make-prediction-template mask layout symbol-ids)
  (let ((items '()) (order '()) (groups (make-table test: eq?)))
    (compiler-index-set-for-each
     mask
     (lambda (production-id)
       (let* ((item (make-core-item production-id 0 layout))
              (id (vector-ref symbol-ids item)))
         (set! items (cons item items))
         (when id
           (let (row (table-ref groups id #f))
             (unless row
               (set! row (vector item id 0))
               (set! order (cons id order))
               (table-set! groups id row))
             (vector-set! row 2
                          (compiler-index-set-add (vector-ref row 2) (+ item 1))))))))
    (vector (reverse items)
            (map (lambda (id) (table-ref groups id)) (reverse order))
            groups)))

(def (lr0-closure kernel-mask template)
  (let (seed '())
    (compiler-index-set-for-each
     kernel-mask (lambda (item) (set! seed (cons item seed))))
    ;; Retain the immutable template suffix when the kernel is exhausted.
    (let merge ((left (reverse seed)) (right (vector-ref template 0))
                (found '()))
      (cond
       ((null? left) (foldl cons right found))
       ((null? right) (foldl cons left found))
       ((= (car left) (car right))
        (merge (cdr left) (cdr right) (cons (car left) found)))
       ((< (car left) (car right))
        (merge (cdr left) right (cons (car left) found)))
       (else (merge left (cdr right) (cons (car right) found)))))))

(def (lr0-state-kernels kernel-mask template symbol-ids masks first-items)
  (let ((order '()) (groups (vector-ref template 2)))
    ;; Only advanced kernel items contribute new groups or alter template
    ;; groups. The full closure is never hashed or grouped again per state.
    (compiler-index-set-for-each
     kernel-mask
     (lambda (item)
       (let (id (vector-ref symbol-ids item))
         (when id
           (when (zero? (vector-ref masks id))
             (set! order (cons id order))
             (vector-set! first-items id item))
           (vector-set! masks id
                        (compiler-index-set-add (vector-ref masks id) (+ item 1)))))))
    (let (own
          (list-sort
           (lambda (left right) (< (vector-ref left 0) (vector-ref right 0)))
           (map
            (lambda (id)
              (let ((shared (table-ref groups id #f))
                    (first (vector-ref first-items id))
                    (mask (vector-ref masks id)))
                (if shared
                  (vector (min first (vector-ref shared 0)) id
                          (compiler-index-set-union mask (vector-ref shared 2)))
                  (vector first id mask))))
            order)))
      ;; Earliest-item order reproduces the original sorted-closure symbol
      ;; discovery order, including kernel symbols that precede predictions.
      (let (rows
            (let merge ((left own) (right (vector-ref template 1)) (found '()))
              (cond
               ((and (pair? right)
                     (not (zero? (vector-ref masks (vector-ref (car right) 1)))))
                (merge left (cdr right) found))
               ((null? right) (foldl cons left found))
               ((null? left) (merge left (cdr right) (cons (car right) found)))
               ((< (vector-ref (car left) 0) (vector-ref (car right) 0))
                (merge (cdr left) right (cons (car left) found)))
               (else (merge left (cdr right) (cons (car right) found))))))
        (for-each (lambda (id) (vector-set! masks id 0)) order)
        rows))))

;;; Owns LR(0) state interning and transition discovery as one work queue.
;;; Each state is published once; vector growth preserves assigned indices.
;; build-lr0-automaton
;; : (forall (p) (-> [p] Vector (Pair Fixnum Fixnum) List))
;; build-lr0-automaton
;;   : (-> List Vector Pair List)
;;   | doc m%
;;       `build-lr0-automaton` interns states and transitions in one queue drain.
;;
;;       # Examples
;;
;;       ```scheme
;;       (build-lr0-automaton productions table layout)
;;       ;; => stable states, transitions, indexes, and visit count
;;       ```
;;     %
(def (build-lr0-automaton productions table layout core-symbols)
  (let* ((productions-by-lhs (production-index-by-lhs productions))
         (prediction-catalog
          (make-core-prediction-catalog core-symbols productions-by-lhs layout))
         (initial-kernel (compiler-index-set-add 0 (make-core-item 0 0 layout)))
         (templates (make-table test: equal?))
         (states (make-vector 128 #f))
         (state-transitions (make-vector 128 '()))
         (kernel-plans (make-vector 128 #f))
         (state-index (make-table test: equal?))
         (state-count 1)
         (processed-count 0)
         (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (let-values (((symbols symbol-ids) (make-lr0-symbol-catalog core-symbols)))
     (let ((masks (make-vector (vector-length symbols) 0))
           (first-items (make-vector (vector-length symbols) 0)))
    (def (template-for kernel)
      (let (mask 0)
        (compiler-index-set-for-each
         kernel
         (lambda (item)
           (set! mask (compiler-index-set-union
                       mask (vector-ref prediction-catalog item)))))
        (or (table-ref templates mask #f)
            (let (template (make-prediction-template mask layout symbol-ids))
              (table-set! templates mask template)
              template))))
    (let (template (template-for initial-kernel))
      (vector-set! states 0 (lr0-closure initial-kernel template))
      (vector-set! kernel-plans 0 (cons initial-kernel template)))
    (table-set! state-index initial-kernel 0)
    (def (grow!)
      (when (= state-count (vector-length states))
        (let ((next-states (make-vector (* 2 state-count) #f))
              (next-transitions (make-vector (* 2 state-count) '()))
              (next-plans (make-vector (* 2 state-count) #f)))
          (let copy ((index 0))
            (when (< index state-count)
              (vector-set! next-states index (vector-ref states index))
              (vector-set! next-transitions index
                           (vector-ref state-transitions index))
              (vector-set! next-plans index (vector-ref kernel-plans index))
              (copy (+ index 1))))
          (set! states next-states)
          (set! state-transitions next-transitions)
          (set! kernel-plans next-plans))))
    (def (append-state! kernel)
      (grow!)
      (let ((index state-count) (template (template-for kernel)))
        (vector-set! states index (lr0-closure kernel template))
        (vector-set! kernel-plans index (cons kernel template))
        (table-set! state-index kernel index)
        (set! state-count (+ state-count 1))
        index))
    (let (queue (make-Queue))
      (enqueue! queue 0)
      (let loop ()
        (if (queue-empty? queue)
          (values states state-transitions state-count productions-by-lhs
                  processed-count)
          (let (index (dequeue! queue))
          (set! processed-count (+ processed-count 1))
          (when (and trace? (zero? (modulo processed-count 100)))
            (display "[gerbil-parser-lr] lr0-states=")
            (display state-count)
            (display " processed=")
            (displayln processed-count)
            (force-output))
          ;; Closure adds only dot-zero items. Every noninitial state is
          ;; therefore identified exactly by its advanced-item kernel mask;
          ;; the augmented dot-zero initial kernel has a disjoint key.
          (let ((rows '()) (plan (vector-ref kernel-plans index)))
            (for-each
             (lambda (kernel)
               (let* ((symbol (vector-ref symbols (vector-ref kernel 1)))
                      (mask (vector-ref kernel 2))
                      (existing (table-ref state-index mask #f))
                      (target-index
                       (or existing
                           (append-state! mask))))
                 (set! rows (cons (cons symbol target-index) rows))
                 (unless existing (enqueue! queue target-index))))
             (lr0-state-kernels (car plan) (cdr plan) symbol-ids masks first-items))
            ;; There is exactly one kernel per symbol; publish the row once.
            (vector-set! state-transitions index (reverse rows)))
            (loop)))))))))
