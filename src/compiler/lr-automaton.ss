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
        materialize-transitions
        transition-index transition-target)

;; make-item-layout
;; : (-> Vector Vector (Pair Fixnum Fixnum))
(def (make-item-layout table terminal-values)
  (cons (vector-length terminal-values)
        (foldl (lambda (production dot-width)
                 (max dot-width (+ (length (production-rhs production)) 1)))
               1
               (vector->list table))))

(def (item-lookahead item layout) (modulo item (car layout)))
(def (item-body item layout) (quotient item (car layout)))
(def (item-production-id item layout)
  (quotient (item-body item layout) (cdr layout)))
(def (item-dot item layout) (modulo (item-body item layout) (cdr layout)))

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
  (+ dot (* (cdr layout) production-id)))
(def (core-item-production-id item layout) (quotient item (cdr layout)))
(def (core-item-dot item layout) (modulo item (cdr layout)))

;; Pre-index every valid dotted production position once. LR closure,
;; propagation, and action construction then resolve after-dot symbols with one
;; vector-ref instead of repeated quotient/modulo/length/list-ref traversal.
;; : (-> Vector Pair Vector)
(def (make-core-symbol-catalog table layout)
  (let ((catalog (make-vector (* (vector-length table) (cdr layout)) #f)))
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

(def (lr0-closure kernel-mask prediction-catalog layout)
  (let ((mask 0) (seed '()) (predicted '()))
    (compiler-index-set-for-each
     kernel-mask
     (lambda (item)
       (set! seed (cons item seed))
       (set! mask (compiler-index-set-union
                   mask (vector-ref prediction-catalog item)))))
    (compiler-index-set-for-each
     mask
     (lambda (id) (set! predicted (cons (make-core-item id 0 layout) predicted))))
    ;; Prediction ids are dense even when packed core ids have wide dot gaps.
    ;; Merge the sorted kernel with dot-zero predictions in canonical order.
    (let merge ((left (reverse seed)) (right (reverse predicted))
                (found '()))
      (cond
       ((null? left) (reverse (foldl cons found right)))
       ((null? right) (reverse (foldl cons found left)))
       ((= (car left) (car right))
        (merge (cdr left) (cdr right) (cons (car left) found)))
       ((< (car left) (car right))
        (merge (cdr left) right (cons (car left) found)))
       (else (merge left (cdr right) (cons (car right) found)))))))

(def (lr0-state-kernels state core-symbols)
  (let ((kernels (make-table test: equal?)) (symbol-order '()))
    (for-each
     (lambda (item)
       (let (symbol (vector-ref core-symbols item))
         (when symbol
           (let (known (table-ref kernels symbol #f))
             (unless known (set! symbol-order (cons symbol symbol-order)))
             (table-set! kernels symbol
                         (compiler-index-set-add (or known 0) (+ item 1)))))))
     state)
    (map (lambda (symbol) (cons symbol (table-ref kernels symbol)))
         (reverse symbol-order))))

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
         (initial (lr0-closure initial-kernel
                               prediction-catalog layout))
         (states (make-vector 128 #f))
         (state-transitions (make-vector 128 '()))
         (state-index (make-table test: equal?))
         (state-count 1)
         (processed-count 0)
         (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (vector-set! states 0 initial)
    (table-set! state-index initial-kernel 0)
    (def (grow!)
      (when (= state-count (vector-length states))
        (let ((next-states (make-vector (* 2 state-count) #f))
              (next-transitions (make-vector (* 2 state-count) '())))
          (let copy ((index 0))
            (when (< index state-count)
              (vector-set! next-states index (vector-ref states index))
              (vector-set! next-transitions index
                           (vector-ref state-transitions index))
              (copy (+ index 1))))
          (set! states next-states)
          (set! state-transitions next-transitions))))
    (def (append-state! kernel state)
      (grow!)
      (let (index state-count)
        (vector-set! states index state)
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
          (let (rows '())
            (for-each
             (lambda (kernel)
               (let* ((symbol (car kernel))
                      (mask (cdr kernel))
                      (existing (table-ref state-index mask #f))
                      (target-index
                       (or existing
                           (append-state!
                            mask (lr0-closure mask prediction-catalog layout)))))
                 (set! rows (cons (cons symbol target-index) rows))
                 (unless existing (enqueue! queue target-index))))
             (lr0-state-kernels (vector-ref states index) core-symbols))
            ;; There is exactly one kernel per symbol; publish the row once.
            (vector-set! state-transitions index (reverse rows)))
            (loop)))))))
