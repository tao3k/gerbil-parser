;;; -*- Gerbil -*-
;;; LALR lookahead fixed-point propagation over an interned LR(0) graph.

(import (prefix-in :std/struct/queue stdq-)
        (only-in :std/vector/extensible
                 list->ExtensibleVector
                 ExtensibleVector->vector ExtensibleVector-push!
                 ExtensibleVector-ref ExtensibleVector-set!)
        (only-in ./funcs
                 compiler-index-set-add
                 compiler-index-set-difference compiler-index-set-empty?
                 compiler-index-set-singleton compiler-index-set-union)
        (only-in ./lr
                 +lr-eof+ nonterminal-name nonterminal-symbol? production-id
                 production-index-by-lhs
                 production-rhs production-terminal-catalog)
        (only-in ./lr-automaton
                 build-lr0-automaton
                 make-core-item
                 make-core-symbol-catalog make-item-layout
                 materialize-transitions))
(export build-states-via-lr0 build-states-via-canonical-lr1
        make-core-lookahead-catalog)

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

;;; FIRST of a concatenation is FIRST(head), plus FIRST(tail) when head
;; is nullable. Fold this equation right-to-left once per production, instead
;; of rescanning every nullable suffix. Cache each nonterminal's completed FIRST
;; mask once for this catalog. The core symbols have already unwrapped marks.
;; : (-> Vector Pair Vector Table Table Table (values Vector Vector))
(def (make-core-lookahead-catalog table layout core-symbols first nullable
                                  terminal-index)
  (let* ((size (* (vector-length table) (cdr layout)))
         (first-masks (make-vector size 0))
         (nullable-tails (make-vector size #f))
         (nonterminal-masks (make-table test: eq?)))
    (def (first-mask name)
      (let (known (table-ref nonterminal-masks name #f))
        (or known
            (let (mask (terminal-list->mask (table-ref first name '()) terminal-index))
              (table-set! nonterminal-masks name mask)
              mask))))
    (let production-loop ((production-id 0))
      (when (< production-id (vector-length table))
        (let item-loop
            ((dot (- (length (production-rhs (vector-ref table production-id))) 1))
             (tail-mask 0) (tail-nullable? #t))
          (when (>= dot 0)
            (let* ((item (make-core-item production-id dot layout))
                   (symbol (vector-ref core-symbols item)))
              (if (nonterminal-symbol? symbol)
                (let* ((name (nonterminal-name symbol))
                       (head-nullable? (table-ref nullable name #f))
                       (head-mask (first-mask name)))
                  (vector-set! first-masks item tail-mask)
                  (vector-set! nullable-tails item tail-nullable?)
                  (item-loop (- dot 1)
                             (if head-nullable?
                               (compiler-index-set-union head-mask tail-mask)
                               head-mask)
                             (and head-nullable? tail-nullable?)))
                (item-loop (- dot 1)
                           (compiler-index-set-singleton
                            (table-ref terminal-index symbol)) #f)))))
        (production-loop (+ production-id 1))))
    (values first-masks nullable-tails)))

;; Assign every admitted (state, core-item) pair one dense node id.  The fixed
;; point and immutable graph construction both use these contiguous slices.
;; : (-> Vector Fixnum (values Vector Vector Fixnum))
(def (index-lr0-items states state-count)
  (let ((offsets (make-vector (+ state-count 1) 0))
        (node-count 0))
    (let count-states ((state 0))
      (when (< state state-count)
        (vector-set! offsets state node-count)
        (set! node-count
              (+ node-count (length (vector-ref states state))))
        (count-states (+ state 1))))
    (vector-set! offsets state-count node-count)
    (let (node-items (make-vector node-count 0))
      (let index-states ((state 0))
        (when (< state state-count)
          (let ((node (vector-ref offsets state)))
            (for-each
             (lambda (item)
               (vector-set! node-items node item)
               (set! node (+ node 1)))
             (vector-ref states state)))
          (index-states (+ state 1))))
      (values offsets node-items node-count))))


;; State item rows are sorted contiguous slices. Binary search locates a node
;; without allocating or hashing a packed (state, item) key.
(def (lookahead-node-index offsets node-items state item)
  (let search ((low (vector-ref offsets state))
               (high (vector-ref offsets (+ state 1))))
    (if (= low high)
      #f
      (let* ((middle (quotient (+ low high) 2))
             (found (vector-ref node-items middle)))
        (cond
         ((= found item) middle)
         ((< found item) (search (+ middle 1) high))
         (else (search low middle)))))))

;; Resolve the immutable propagation graph through sorted state item slices.
;; Fixed-point updates then touch vectors only.
(def (make-lookahead-propagation-graph
      state-transitions productions-by-lhs core-symbols layout
      node-items offsets)
  (let* ((node-count (vector-length node-items))
         (closure-targets (make-vector node-count '()))
         (shift-targets (make-vector node-count #f))
         (symbol-index (make-table test: equal?))
         (symbol-count 0)
         (core-symbol-ids (make-vector (vector-length core-symbols) #f)))
    (let index-symbols ((item 0))
      (when (< item (vector-length core-symbols))
        (let (symbol (vector-ref core-symbols item))
          (when symbol
            (let* ((known (table-ref symbol-index symbol #f))
                   (id (or known symbol-count)))
              (unless known
                (table-set! symbol-index symbol id)
                (set! symbol-count (+ symbol-count 1)))
              (vector-set! core-symbol-ids item id))))
        (index-symbols (+ item 1))))
    (let ((targets (make-vector symbol-count #f))
          (prediction-targets (make-vector symbol-count #f)))
     (let state-loop ((state 0))
      (when (< state (- (vector-length offsets) 1))
          (let (prediction-order '())
            (for-each
             (lambda (row)
               (let (id (table-ref symbol-index (car row) #f))
                 (unless id (error "LR transition symbol is not indexed" state row))
                 (vector-set! targets id (cdr row))))
             (vector-ref state-transitions state))
            (let node-loop ((node (vector-ref offsets state)))
              (when (< node (vector-ref offsets (+ state 1)))
                (let* ((item (vector-ref node-items node))
                       (symbol (vector-ref core-symbols item)))
                  (when (and symbol (nonterminal-symbol? symbol))
                    (let* ((id (vector-ref core-symbol-ids item))
                           (known (vector-ref prediction-targets id))
                           (predictions
                            (or known
                                (map
                                 (lambda (production)
                                   (let* ((target-item (make-core-item (production-id production) 0 layout))
                                          (target (lookahead-node-index offsets node-items state target-item)))
                                     (unless target
                                       (error "LR(0) closure target is not indexed" state item target-item))
                                     target))
                                 (table-ref productions-by-lhs (nonterminal-name symbol) '())))))
                      (unless known
                        (vector-set! prediction-targets id predictions)
                        (set! prediction-order (cons id prediction-order)))
                      (vector-set! closure-targets node predictions)))
                  (when symbol
                    (let* ((target-state (vector-ref targets (vector-ref core-symbol-ids item)))
                           (target (and target-state
                                        (lookahead-node-index offsets node-items target-state (+ item 1)))))
                      (unless target
                        (error "LR(0) shift target is not indexed" state item symbol target-state))
                      (vector-set! shift-targets node target))))
                (node-loop (+ node 1))))
            ;; Stored adjacency lists are immutable; discard only scratch
            ;; references so memory grows with edges, not states times symbols.
            (for-each (lambda (id) (vector-set! prediction-targets id #f)) prediction-order)
            (for-each
             (lambda (row)
               (vector-set! targets (table-ref symbol-index (car row)) #f))
             (vector-ref state-transitions state)))
        (state-loop (+ state 1)))))
    (values closure-targets shift-targets)))

;;; Propagates terminal masks to a fixed point over the interned LR(0) graph.
;;; Dense vectors own both graph identity and fixed-point mutable state.
;; propagate-lalr-lookaheads
;; : (-> Vector Vector Fixnum Table Vector Vector Table Table Vector Table Pair List)
(def (propagate-lalr-lookaheads states state-transitions state-count
                                productions-by-lhs table core-symbols
                                tail-first-masks nullable-tails
                                terminal-index layout)
  (let* ((queue (stdq-make-Queue))
         (processed-count 0)
         (started (##current-time-point))
         (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (let-values (((offsets node-items node-count)
                  (index-lr0-items states state-count)))
      (trace-lookahead-phase 'lookahead-index node-count started trace?)
      (let-values (((closure-targets shift-targets)
                    (make-lookahead-propagation-graph
                     state-transitions productions-by-lhs core-symbols layout
                     node-items offsets)))
       (trace-lookahead-phase 'lookahead-graph node-count started trace?)
       (let ((lookaheads (make-vector node-count 0))
             (pending (make-vector node-count 0))
             (queued (make-vector node-count #f))
             (activated (make-vector node-count #f)))
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
         (lookahead-node-index offsets node-items 0 (make-core-item 0 0 layout))
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
                (let* ((first? (not (vector-ref activated node)))
                       (nullable? (vector-ref nullable-tails item))
                       (evidence
                        (cond
                         (first?
                          (if nullable?
                            (compiler-index-set-union
                             (vector-ref tail-first-masks item) delta)
                            (vector-ref tail-first-masks item)))
                         (nullable? delta)
                         (else 0))))
                  ;; FIRST(tail) is static; subsequent activations propagate
                  ;; only new lookahead bits, and only through nullable tails.
                  (vector-set! activated node #t)
                  (unless (zero? evidence)
                    (for-each
                     (lambda (target) (enqueue-mask! target evidence))
                     (vector-ref closure-targets node)))))
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

;;; Canonical LR(1) is the semantic reference for conflict-directed state
;;; partitioning. A state contains one mask per LR(0) core; closure propagates
;;; FIRST(tail) and the incoming follow mask until stable. State identity
;;; includes those masks, so contexts that LALR merges remain distinguishable.
;;; The representation deliberately matches build-states-via-lr0's result.
(def (build-states-via-canonical-lr1 productions table first nullable
                                     (state-limit #f))
  (call-with-current-continuation
   (lambda (abort)
    (let-values (((terminal-values terminal-index)
                (production-terminal-catalog productions)))
    (let* ((layout (make-item-layout table terminal-values))
           (core-symbols (make-core-symbol-catalog table layout))
           (productions-by-lhs (production-index-by-lhs productions))
           (states (list->ExtensibleVector '()))
           (state-transitions (list->ExtensibleVector '()))
           (state-index (make-table test: equal?))
           (closure-masks (make-vector (vector-length core-symbols) 0))
           (state-count 0))
      (let-values (((tail-first-masks nullable-tails)
                    (make-core-lookahead-catalog
                     table layout core-symbols first nullable terminal-index)))
        (def (canonical-closure kernels)
          (let ((cores (list->ExtensibleVector '()))
                (pending (stdq-make-Queue)))
            (def (admit! core mask)
              (let* ((old (vector-ref closure-masks core))
                     (next (compiler-index-set-union old mask)))
                (unless (= old next)
                  (when (zero? old)
                    (ExtensibleVector-push! cores core))
                  (vector-set! closure-masks core next)
                  (stdq-enqueue! pending core))))
            (for-each (lambda (entry) (admit! (car entry) (cdr entry)))
                      kernels)
            (let loop ()
              (unless (stdq-queue-empty? pending)
                (let* ((core (stdq-dequeue! pending))
                       (mask (vector-ref closure-masks core))
                       (symbol (vector-ref core-symbols core)))
                  (when (and symbol (nonterminal-symbol? symbol))
                    (let (evidence
                          (if (vector-ref nullable-tails core)
                            (compiler-index-set-union
                             (vector-ref tail-first-masks core) mask)
                            (vector-ref tail-first-masks core)))
                      (for-each
                       (lambda (production)
                         (admit!
                          (make-core-item (production-id production) 0 layout)
                          evidence))
                       (table-ref productions-by-lhs
                                  (nonterminal-name symbol) '()))))
                  (loop))))
            (let (result
                  (list-sort
                   (lambda (left right) (< (car left) (car right)))
                   (map (lambda (core)
                          (let (mask (vector-ref closure-masks core))
                            (vector-set! closure-masks core 0)
                            (cons core mask)))
                        (vector->list (ExtensibleVector->vector cores)))))
              result)))
        (def (intern! state)
          (let (existing (table-ref state-index state #f))
            (if existing
              (values existing #f)
              (let (index
                    (begin
                      (when (and state-limit (>= state-count state-limit))
                        (abort #f))
                      (ExtensibleVector-push! states state)))
                (ExtensibleVector-push! state-transitions '())
                (table-set! state-index state index)
                (set! state-count (+ state-count 1))
                (values index #t)))))
        (let* ((eof-mask
                (compiler-index-set-singleton
                 (table-ref terminal-index +lr-eof+)))
               (initial
                (canonical-closure
                 (list (cons (make-core-item 0 0 layout) eof-mask))))
               (queue (stdq-make-Queue)))
          (intern! initial)
          (stdq-enqueue! queue 0)
          (let drain ()
            (unless (stdq-queue-empty? queue)
              (let* ((index (stdq-dequeue! queue))
                     (state (ExtensibleVector-ref states index))
                     (kernels (make-table test: equal?))
                     (kernel-cores (make-table test: equal?))
                     (symbol-order '()))
                (for-each
                 (lambda (entry)
                   (let ((symbol (vector-ref core-symbols (car entry))))
                     (when symbol
                       (let (items (table-ref kernels symbol #f))
                         (unless items
                           (set! items (make-table test: eq?))
                           (table-set! kernels symbol items)
                           (set! symbol-order (cons symbol symbol-order)))
                         (let* ((next-core (+ (car entry) 1))
                                (old (table-ref items next-core 0)))
                           (when (zero? old)
                             (table-set! kernel-cores symbol
                                         (cons next-core
                                               (table-ref kernel-cores symbol '()))))
                           (table-set!
                            items next-core
                            (compiler-index-set-union old (cdr entry))))))))
                 state)
                (for-each
                 (lambda (symbol)
                   (let* ((items (table-ref kernels symbol))
                          (target
                           (canonical-closure
                            (map (lambda (core)
                                   (cons core (table-ref items core 0)))
                                 (table-ref kernel-cores symbol '())))))
                     (let-values (((target-index novel?) (intern! target)))
                       (ExtensibleVector-set!
                        state-transitions index
                        (cons (cons symbol target-index)
                              (ExtensibleVector-ref state-transitions index)))
                       (when novel?
                         (stdq-enqueue! queue target-index)))))
                 (reverse symbol-order))
                (ExtensibleVector-set!
                 state-transitions index
                 (reverse (ExtensibleVector-ref state-transitions index)))
                (drain))))
          (let ((core-states (make-vector state-count #f))
                (offsets (make-vector (+ state-count 1) 0))
                (node-count 0))
            (let state-loop ((index 0))
              (when (< index state-count)
                (let (state (ExtensibleVector-ref states index))
                  (vector-set! offsets index node-count)
                  (vector-set! core-states index (map car state))
                  (set! node-count (+ node-count (length state))))
                (state-loop (+ index 1))))
            (vector-set! offsets state-count node-count)
            (let ((lookaheads (make-vector node-count 0))
                  (node 0))
              (let fill ((index 0))
                (when (< index state-count)
                  (for-each
                   (lambda (entry)
                     (vector-set! lookaheads node (cdr entry))
                     (set! node (+ node 1)))
                   (ExtensibleVector-ref states index))
                  (fill (+ index 1))))
              (values core-states state-count lookaheads offsets
                      (materialize-transitions
                       (ExtensibleVector->vector state-transitions)
                       state-count)
                      terminal-values layout core-symbols
                      state-count node-count))))))))))
