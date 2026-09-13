;;; -*- Gerbil -*-
;;; LALR lookahead fixed-point propagation over an interned LR(0) graph.

(import (prefix-in :std/misc/queue stdq-)
        (only-in ./funcs
                 compiler-terminal-set-add
                 compiler-terminal-set-difference compiler-terminal-set-empty?
                 compiler-terminal-set-for-each
                 compiler-terminal-set-singleton compiler-terminal-set-union)
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

;; lookahead-mask->list
;; : (-> Fixnum List)
;; : (-> List Table Integer)
(def (terminal-list->mask terminals terminal-index)
  (foldl (lambda (terminal mask)
           (compiler-terminal-set-add
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

;;; Propagates terminal masks to a fixed point over the interned LR(0) graph.
;;; Queue membership coalesces repeated evidence; output waits for a full drain.
;; propagate-lalr-lookaheads
;; : (-> Vector Vector Fixnum Table Vector Vector Table Table Vector Table Pair List)
(def (propagate-lalr-lookaheads states state-transitions state-count
                                productions-by-lhs table core-symbols
                                tail-first-masks nullable-tails
                                terminal-index layout)
  (let ((lookaheads (make-table test: eq?))
        (pending (make-table test: eq?))
        (queued (make-table test: eq?))
        (queue (stdq-make-queue))
        (item-space (* (cdr layout) (vector-length table)))
        (processed-count 0)
        (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (def (item-key state item) (+ item (* item-space state)))
    (def (enqueue-mask! state item evidence)
      (let* ((key (item-key state item))
             (known (table-ref lookaheads key 0))
             (novel
              (compiler-terminal-set-difference evidence known)))
        (unless (compiler-terminal-set-empty? novel)
          (table-set! lookaheads key
                      (compiler-terminal-set-union known novel))
          (table-set! pending key
                      (compiler-terminal-set-union
                       (table-ref pending key 0) novel))
          (unless (table-ref queued key #f)
            (table-set! queued key #t)
            (stdq-enqueue! queue key)))))
    (enqueue-mask!
     0 (make-core-item 0 0 layout)
     (compiler-terminal-set-singleton
      (table-ref terminal-index +lr-eof+)))
    (let loop ()
      (unless (stdq-queue-empty? queue)
        (let (key (stdq-dequeue! queue))
          (table-set! queued key #f)
          (let* ((state (quotient key item-space))
                 (item (modulo key item-space))
                 (delta (table-ref pending key 0))
                 (symbol (vector-ref core-symbols item)))
            (table-set! pending key 0)
            (set! processed-count (+ processed-count 1))
            (when (and trace? (zero? (modulo processed-count 10000)))
              (display "[gerbil-parser-lr] lookahead-items=")
              (displayln processed-count)
              (force-output))
            (when (and symbol (nonterminal-symbol? symbol))
              (let (evidence
                    (if (vector-ref nullable-tails item)
                      (compiler-terminal-set-union
                       (vector-ref tail-first-masks item) delta)
                      (vector-ref tail-first-masks item)))
                (let production-loop
                    ((productions
                      (table-ref productions-by-lhs
                                 (nonterminal-name symbol) '())))
                  (unless (null? productions)
                    (let (target-item
                          (make-core-item
                           (production-id (car productions)) 0 layout))
                      (enqueue-mask! state target-item evidence))
                    (production-loop (cdr productions))))))
            (when symbol
              (let (transition
                    (assoc symbol (vector-ref state-transitions state)))
                (unless transition
                  (error "LR(0) transition missing during lookahead propagation"
                         state item symbol))
                (enqueue-mask! (cdr transition) (+ item 1) delta))))
          (loop))))
    (values lookaheads item-space processed-count)))

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
        (let-values (((lookaheads item-space lookahead-item-visit-count)
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
          (values states state-count lookaheads item-space
                  (materialize-transitions state-transitions state-count)
                  terminal-values layout core-symbols
                  lr0-state-visit-count lookahead-item-visit-count)))))))
