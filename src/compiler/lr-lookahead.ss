;;; -*- Gerbil -*-
;;; LALR lookahead fixed-point propagation over an interned LR(0) graph.

(import (prefix-in :std/misc/queue stdq-)
        (only-in ./funcs
                 compiler-index-set-add
                 compiler-index-set-difference compiler-index-set-empty?
                 compiler-index-set-singleton compiler-index-set-union)
        (only-in ./lr
                 +lr-eof+ nonterminal-name nonterminal-symbol? production-id
                 production-rhs production-terminal-catalog sequence-first
                 sequence-nullable?)
        (only-in ./lr-automaton
                 build-lr0-automaton
                 make-core-item
                 make-core-symbol-catalog make-item-layout
                 materialize-transitions))
(export build-states-via-lr0)

(def (trace-lookahead-phase phase count started trace?)
  (when trace?
    (display "[gerbil-parser-lr] phase=")
    (display phase)
    (display " count=")
    (display count)
    (display " elapsed-ms=")
    (displayln
     (inexact->exact
      (floor (* 1000.0 (- (##current-time-point) started)))))
    (force-output)))

;; lookahead-mask->list
;; : (-> Fixnum List)
;; : (-> List Table Integer)
(def (terminal-list->mask terminals terminal-index)
  (foldl (lambda (terminal mask)
           (compiler-index-set-add
            mask (table-ref terminal-index terminal)))
         0 terminals))

;; FIRST(tail) and tail-nullability depend only on the dotted core item. Build
;; them once for every production position before the fixed-point queue drains.
;; : (-> Vector Pair Vector Table Table Table (values Vector Vector))
(def (make-core-lookahead-catalog table layout core-symbols first nullable
                                  terminal-index)
  (let* ((size (* (vector-length table) (cdr layout)))
         (first-masks (make-vector size 0))
         (nullable-tails (make-vector size #f)))
    (let production-loop ((production-id 0))
      (when (< production-id (vector-length table))
        (let item-loop
            ((rest (production-rhs (vector-ref table production-id)))
             (dot 0))
          (unless (null? rest)
            (let (item (make-core-item production-id dot layout))
              (when (nonterminal-symbol? (vector-ref core-symbols item))
                (let (tail (cdr rest))
                  (vector-set!
                   first-masks item
                   (terminal-list->mask
                    (sequence-first tail first nullable) terminal-index))
                  (vector-set! nullable-tails item
                               (sequence-nullable? tail nullable)))))
            (item-loop (cdr rest) (+ dot 1))))
        (production-loop (+ production-id 1))))
    (values first-masks nullable-tails)))

;; Assign every admitted (state, core-item) pair one dense node id.  The fixed
;; point then uses vectors for its mutable state; the packed-key table is paid
;; only while constructing the immutable propagation graph.
;; : (-> Vector Fixnum Fixnum (values Vector Vector Vector Table Fixnum))
(def (index-lr0-items states state-count item-space)
  (let ((offsets (make-vector (+ state-count 1) 0))
        (node-count 0))
    (let count-states ((state 0))
      (when (< state state-count)
        (vector-set! offsets state node-count)
        (set! node-count
              (+ node-count (length (vector-ref states state))))
        (count-states (+ state 1))))
    (vector-set! offsets state-count node-count)
    (let ((node-items (make-vector node-count 0))
          (node-states (make-vector node-count 0))
          (node-index (make-table test: eq?)))
      (let index-states ((state 0))
        (when (< state state-count)
          (let ((node (vector-ref offsets state)))
            (for-each
             (lambda (item)
               (vector-set! node-items node item)
               (vector-set! node-states node state)
               (table-set! node-index (+ item (* item-space state)) node)
               (set! node (+ node 1)))
             (vector-ref states state)))
          (index-states (+ state 1))))
      (values offsets node-items node-states node-index node-count))))

;; Resolve the immutable propagation graph once through the packed-key index.
;; Fixed-point updates then touch vectors only.
(def (make-lookahead-propagation-graph
      state-transitions productions-by-lhs core-symbols layout item-space
      node-items node-states node-index)
  (let* ((node-count (vector-length node-items))
         (closure-targets (make-vector node-count '()))
         (shift-targets (make-vector node-count #f)))
    (let node-loop ((node 0))
      (when (< node node-count)
        (let* ((state (vector-ref node-states node))
               (item (vector-ref node-items node))
               (symbol (vector-ref core-symbols item)))
          (when (and symbol (nonterminal-symbol? symbol))
            (vector-set!
             closure-targets node
             (map (lambda (production)
                    (let* ((target-item
                            (make-core-item
                             (production-id production) 0 layout))
                           (target
                            (table-ref node-index
                                       (+ target-item (* item-space state)) #f)))
                      (unless target
                        (error "LR(0) closure target is not indexed"
                               state item target-item))
                      target))
                  (table-ref productions-by-lhs
                             (nonterminal-name symbol) '()))))
          (when symbol
            (let (transition
                  (assoc symbol (vector-ref state-transitions state)))
              (unless transition
                (error "LR(0) transition missing during lookahead indexing"
                       state item symbol))
              (let (target
                    (table-ref node-index
                               (+ (+ item 1) (* item-space (cdr transition)))
                               #f))
                (unless target
                  (error "LR(0) shift target is not indexed"
                         state item symbol (cdr transition)))
                (vector-set! shift-targets node target))))
          (node-loop (+ node 1)))))
    (values closure-targets shift-targets)))

;;; Propagates terminal masks to a fixed point over the interned LR(0) graph.
;;; Dense vectors replace three hash tables and quotient/modulo decoding. A
;;; single packed-key index is retained only to resolve immutable graph edges.
;; propagate-lalr-lookaheads
;; : (-> Vector Vector Fixnum Table Vector Vector Table Table Vector Table Pair List)
(def (propagate-lalr-lookaheads states state-transitions state-count
                                productions-by-lhs table core-symbols
                                tail-first-masks nullable-tails
                                terminal-index layout)
  (let* ((item-space (* (cdr layout) (vector-length table)))
         (queue (stdq-make-queue))
         (processed-count 0)
         (started (##current-time-point))
         (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (let-values (((offsets node-items node-states node-index node-count)
                  (index-lr0-items states state-count item-space)))
      (trace-lookahead-phase 'lookahead-index node-count started trace?)
      (let-values (((closure-targets shift-targets)
                    (make-lookahead-propagation-graph
                     state-transitions productions-by-lhs core-symbols layout
                     item-space node-items node-states node-index)))
       (trace-lookahead-phase 'lookahead-graph node-count started trace?)
       (let ((lookaheads (make-vector node-count 0))
             (pending (make-vector node-count 0))
             (queued (make-vector node-count #f)))
        (def (enqueue-mask! node evidence)
          (let* ((known (vector-ref lookaheads node))
                 (novel
                  (compiler-index-set-difference evidence known)))
            (unless (compiler-index-set-empty? novel)
              (vector-set! lookaheads node
                           (compiler-index-set-union known novel))
              (vector-set! pending node
                           (compiler-index-set-union
                            (vector-ref pending node) novel))
              (unless (vector-ref queued node)
                (vector-set! queued node #t)
                (stdq-enqueue! queue node)))))
        (enqueue-mask!
         (table-ref node-index (make-core-item 0 0 layout))
         (compiler-index-set-singleton
          (table-ref terminal-index +lr-eof+)))
        (let loop ()
          (unless (stdq-queue-empty? queue)
            (let* ((node (stdq-dequeue! queue))
                   (item (vector-ref node-items node))
                   (delta (vector-ref pending node)))
              (vector-set! queued node #f)
              (vector-set! pending node 0)
              (set! processed-count (+ processed-count 1))
              (when (and trace? (zero? (modulo processed-count 10000)))
                (display "[gerbil-parser-lr] lookahead-items=")
                (displayln processed-count)
                (force-output))
              (unless (null? (vector-ref closure-targets node))
                (let (evidence
                      (if (vector-ref nullable-tails item)
                        (compiler-index-set-union
                         (vector-ref tail-first-masks item) delta)
                        (vector-ref tail-first-masks item)))
                  (for-each
                   (lambda (target) (enqueue-mask! target evidence))
                   (vector-ref closure-targets node))))
              (let (target (vector-ref shift-targets node))
                (when target (enqueue-mask! target delta)))
            (loop))))
        (trace-lookahead-phase
         'lookahead-propagation processed-count started trace?)
        (values lookaheads offsets processed-count))))))

;;; Materializes LALR state identity and lookahead evidence in one transaction.
;; build-states-via-lr0
;; : (-> List Vector Table Table List)
(def (build-states-via-lr0 productions table first nullable)
  (let-values (((terminal-values terminal-index)
                (production-terminal-catalog productions)))
    (let* ((layout (make-item-layout table terminal-values))
           (core-symbols (make-core-symbol-catalog table layout))
           (started (##current-time-point))
           (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
      (let-values (((tail-first-masks nullable-tails)
                    (make-core-lookahead-catalog
                     table layout core-symbols first nullable terminal-index)))
       (let-values (((states state-transitions state-count productions-by-lhs
                             lr0-state-visit-count)
                     (build-lr0-automaton
                      productions table layout core-symbols)))
        (when trace?
          (display "[gerbil-parser-lr] phase=lr0 count=")
          (display state-count)
          (display " elapsed-ms=")
          (displayln
           (inexact->exact
            (floor (* 1000.0 (- (##current-time-point) started)))))
          (force-output))
        (let-values (((lookaheads lookahead-offsets lookahead-item-visit-count)
                      (propagate-lalr-lookaheads
                       states state-transitions state-count productions-by-lhs
                       table core-symbols tail-first-masks nullable-tails
                       terminal-index layout)))
          (when trace?
            (display "[gerbil-parser-lr] phase=lookahead count=")
            (display lookahead-item-visit-count)
            (display " elapsed-ms=")
            (displayln
             (inexact->exact
              (floor (* 1000.0 (- (##current-time-point) started)))))
            (force-output))
          (values states state-count lookaheads lookahead-offsets
                  (materialize-transitions state-transitions state-count)
                  terminal-values layout core-symbols
                  lr0-state-visit-count lookahead-item-visit-count)))))))
