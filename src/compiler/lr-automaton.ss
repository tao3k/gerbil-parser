;;; -*- Gerbil -*-
;;; Packed LR items, transition indexes, and deterministic LR(0) interning.

(import (only-in :std/misc/queue
                 dequeue! enqueue! make-queue queue-empty?)
        (only-in :std/sort sort)
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

(def (transition-set rows symbol target)
  (let (updated? #f)
    (let (next
          (map (lambda (entry)
                 (if (equal? (car entry) symbol)
                   (begin (set! updated? #t) (cons symbol target))
                   entry))
               rows))
      (if updated? next (foldr cons (list (cons symbol target)) rows)))))

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

(def (lr0-closure seed productions-by-lhs core-symbols layout)
  (let (seen (make-table test: eq?))
    (for-each (lambda (item) (table-set! seen item #t)) seed)
    (let loop ((pending seed) (items '()))
      (if (null? pending)
        (sort (reverse items) <)
        (let* ((item (car pending))
               (symbol (vector-ref core-symbols item))
               (expanded (cdr pending)))
          (when (and symbol (nonterminal-symbol? symbol))
            (for-each
             (lambda (production)
               (let (candidate
                     (make-core-item (production-id production) 0 layout))
                 (unless (table-ref seen candidate #f)
                   (table-set! seen candidate #t)
                   (set! expanded (cons candidate expanded)))))
             (table-ref productions-by-lhs (nonterminal-name symbol) '())))
          (loop expanded (cons item items)))))))

(def (lr0-state-kernels state core-symbols)
  (let ((kernels (make-table test: equal?)) (symbol-order '()))
    (for-each
     (lambda (item)
       (let (symbol (vector-ref core-symbols item))
         (when symbol
           (unless (table-ref kernels symbol #f)
             (set! symbol-order (cons symbol symbol-order)))
           (table-set! kernels symbol
                       (cons (+ item 1)
                             (or (table-ref kernels symbol #f) '()))))))
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
         (initial (lr0-closure (list (make-core-item 0 0 layout))
                               productions-by-lhs core-symbols layout))
         (states (make-vector 128 #f))
         (state-transitions (make-vector 128 '()))
         (state-index (make-table test: equal?))
         (state-count 1)
         (processed-count 0)
         (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (vector-set! states 0 initial)
    (table-set! state-index initial 0)
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
    (def (append-state! state)
      (grow!)
      (let (index state-count)
        (vector-set! states index state)
        (table-set! state-index state index)
        (set! state-count (+ state-count 1))
        index))
    (let (queue (make-queue))
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
          (for-each
           (lambda (kernel)
             (let* ((symbol (car kernel))
                    (target (lr0-closure (cdr kernel)
                                         productions-by-lhs core-symbols
                                         layout))
                    (existing (table-ref state-index target #f))
                    (target-index (or existing (append-state! target))))
               (vector-set!
                state-transitions index
                (transition-set (vector-ref state-transitions index)
                                symbol target-index))
               (unless existing
                 (enqueue! queue target-index))))
           (lr0-state-kernels (vector-ref states index) core-symbols))
            (loop)))))))
