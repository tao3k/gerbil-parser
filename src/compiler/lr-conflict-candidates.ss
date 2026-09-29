;;; -*- Gerbil -*-
;;; Conservative raw LR action overlap on the LR(0) propagation graph.

(import (prefix-in :std/struct/queue stdq-)
        (only-in ./funcs
                 compiler-index-set-add compiler-index-set-for-each
                 compiler-index-set-singleton compiler-index-set-union)
        (only-in ./lr
                 +lr-eof+ base-symbol nonterminal-name nonterminal-symbol?
                 production-id production-index-by-lhs production-rhs
                 production-terminal-catalog sequence-first sequence-nullable?
                 terminal-symbol?)
        (only-in ./lr-lookahead build-states-via-lr0))
(export forward-reachable-follow-masks
        initial-backward-follow-partitions
        initial-backward-follow-partitions/from-lr0
        lr0-conflict-candidates raw-conflict-cells)

;; Definition 3.22 specialized to k=1: carry one terminal mask per production
;; and propagate FIRST(tail follow) through nonterminal occurrences. This is
;; independent of canonical LR(1) state construction.
(def (forward-reachable-follow-masks productions table first nullable)
  (let-values (((terminal-values terminal-index)
                (production-terminal-catalog productions)))
    (let* ((count (vector-length table))
           (by-lhs (production-index-by-lhs productions))
           (edges (make-vector count '())))
      (def (first-mask tail)
        (foldl (lambda (terminal mask)
                 (compiler-index-set-add
                  mask (table-ref terminal-index terminal)))
               0 (sequence-first tail first nullable)))
      (let production-loop ((parent 0))
        (when (< parent count)
          (let occurrence-loop ((rest (production-rhs (vector-ref table parent))))
            (unless (null? rest)
              (let (symbol (base-symbol (car rest)))
                (when (nonterminal-symbol? symbol)
                  (let ((tail (cdr rest)))
                    (for-each
                     (lambda (child)
                       (vector-set!
                        edges parent
                        (cons (vector (production-id child)
                                      (first-mask tail)
                                      (sequence-nullable? tail nullable))
                              (vector-ref edges parent))))
                     (table-ref by-lhs (nonterminal-name symbol) '())))))
              (occurrence-loop (cdr rest))))
          (production-loop (+ parent 1))))
      (let ((follows (make-vector count 0))
            (queued (make-vector count #f))
            (pending (stdq-make-Queue)))
        (vector-set! follows 0
                     (compiler-index-set-singleton
                      (table-ref terminal-index +lr-eof+)))
        (vector-set! queued 0 #t)
        (stdq-enqueue! pending 0)
        (let drain ()
          (unless (stdq-queue-empty? pending)
            (let ((parent (stdq-dequeue! pending)))
              (vector-set! queued parent #f)
              (for-each
               (lambda (edge)
                 (let* ((child (vector-ref edge 0))
                        (known (vector-ref follows child))
                        (evidence
                         (if (vector-ref edge 2)
                           (compiler-index-set-union
                            (vector-ref edge 1)
                            (vector-ref follows parent))
                           (vector-ref edge 1)))
                        (next (compiler-index-set-union known evidence)))
                   (unless (= known next)
                     (vector-set! follows child next)
                     (unless (vector-ref queued child)
                       (vector-set! queued child #t)
                       (stdq-enqueue! pending child)))))
               (vector-ref edges parent))
              (drain))))
        (values follows terminal-values)))))

;; Keep the first raw action at each terminal and mark a conflict when a
;; distinct action appears. Shift is action 0, accept is 1, and reduction by
;; production n is n+1. This avoids allocating a production-sized integer
;; bitset for every completed item. Precedence and GLR do not erase this
;; evidence: it is intended for partition construction, not parser execution.
(def (raw-conflict-cells states state-count lookaheads offsets
                         terminal-values layout core-symbols)
  (let ((conflicts (make-vector state-count 0))
        (terminal-count (vector-length terminal-values))
        (terminal-index (make-table test: equal?)))
    (let index-loop ((index 0))
      (when (< index terminal-count)
        (table-set! terminal-index (vector-ref terminal-values index) index)
        (index-loop (+ index 1))))
    (let state-loop ((state 0))
      (when (< state state-count)
        (let ((actions (make-vector terminal-count #f))
              (node (vector-ref offsets state))
              (mask 0))
          (def (record-action! terminal action)
            (let (prior (vector-ref actions terminal))
              (cond
               ((not prior) (vector-set! actions terminal action))
               ((not (= prior action))
                (set! mask (compiler-index-set-add mask terminal))))))
          (for-each
           (lambda (core)
             (let ((symbol (vector-ref core-symbols core))
                   (production-id (quotient core (cdr layout))))
               (cond
                ((and symbol (terminal-symbol? symbol))
                 (let (index (table-ref terminal-index symbol #f))
                   (unless index
                     (error "LR terminal missing from catalogue" symbol))
                   (record-action! index 0)))
                ((not symbol)
                 (let (action
                       (if (zero? production-id)
                         1
                         (+ production-id 1)))
                   (compiler-index-set-for-each
                    (vector-ref lookaheads node)
                    (lambda (terminal)
                      (record-action! terminal action))))))
               (set! node (+ node 1))))
           (vector-ref states state))
          (vector-set! conflicts state mask))
        (state-loop (+ state 1))))
    conflicts))

;; A raw overlap here is a *candidate* for follow-string partitioning. LALR
;; propagation unions contexts, so its overlap can be spurious; conversely it
;; contains every canonical LR(1) conflict after projection to an LR(0) state.
(def (lr0-conflict-candidates productions table first nullable)
  (let-values (((states state-count lookaheads offsets transitions
                        terminal-values layout core-symbols
                        state-visits item-visits)
                (build-states-via-lr0 productions table first nullable)))
    (values states state-count terminal-values
            (raw-conflict-cells states state-count lookaheads offsets
                                terminal-values layout core-symbols))))

;; Definition 3.25's initial backward partition for k=1, conservatively
;; refining against every LR(0)/LALR candidate terminal. The output slot for
;; each valid dotted core contains (compatibility-mask . follow-mask) blocks.
;; This is only the seed: backward and forward edge refinements still follow.
(def (initial-backward-follow-partitions/from-lr0
      productions table first nullable states count terminals candidates)
    (let-values (((follows follow-terminals)
                  (forward-reachable-follow-masks
                   productions table first nullable)))
      (unless (equal? terminals follow-terminals)
        (error "LR terminal catalogues disagree"))
      (let* ((terminal-index (make-table test: equal?))
             (dot-width
              (foldl (lambda (production width)
                       (max width (+ 1 (length (production-rhs production)))))
                     1 (vector->list table)))
             (core-count (* (vector-length table) dot-width))
             (candidate-by-core (make-vector core-count 0))
             (partitions (make-vector core-count #f)))
        (let terminal-loop ((terminal 0))
          (when (< terminal (vector-length terminals))
            (table-set! terminal-index
                        (vector-ref terminals terminal) terminal)
            (terminal-loop (+ terminal 1))))
        (let state-loop ((state 0))
          (when (< state count)
            (for-each
             (lambda (core)
               (vector-set!
                candidate-by-core core
                (compiler-index-set-union
                 (vector-ref candidate-by-core core)
                 (vector-ref candidates state))))
             (vector-ref states state))
            (state-loop (+ state 1))))
        (let production-loop ((id 0))
          (when (< id (vector-length table))
            (let dot-loop ((tail (production-rhs (vector-ref table id)))
                           (dot 0))
              (let* ((core (+ dot (* dot-width id)))
                     (first-mask
                      (foldl
                       (lambda (terminal mask)
                         (compiler-index-set-add
                          mask (table-ref terminal-index terminal)))
                       0 (sequence-first tail first nullable)))
                     (tail-nullable? (sequence-nullable? tail nullable))
                     (candidate-mask (vector-ref candidate-by-core core))
                     (blocks (make-table test: eq?))
                     (signatures '()))
                (compiler-index-set-for-each
                 (vector-ref follows id)
                 (lambda (lookahead)
                   (let* ((compatible
                           (if tail-nullable?
                             (compiler-index-set-add first-mask lookahead)
                             first-mask))
                          (signature
                           (bitwise-and compatible candidate-mask))
                          (known (table-ref blocks signature #f)))
                     (unless known
                       (set! signatures (cons signature signatures)))
                     (table-set!
                      blocks signature
                      (compiler-index-set-add (or known 0) lookahead)))))
                (vector-set!
                 partitions core
                 (map (lambda (signature)
                        (cons signature (table-ref blocks signature)))
                      (reverse signatures)))
                (unless (null? tail)
                  (dot-loop (cdr tail) (+ dot 1)))))
            (production-loop (+ id 1))))
        (values partitions candidates terminals))))

(def (initial-backward-follow-partitions productions table first nullable)
  (let-values (((states count terminals candidates)
                (lr0-conflict-candidates
                 productions table first nullable)))
    (initial-backward-follow-partitions/from-lr0
     productions table first nullable states count terminals candidates)))
