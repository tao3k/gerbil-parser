;;; -*- Gerbil -*-
;;; LR conflict resolution and canonical parser-table publication.

(import (only-in :std/iter for in-range)
        (only-in ./funcs compiler-index-set-for-each)
        (only-in ./lr
                 compute-first compute-nullable
                 lower-rules nonterminal-name nonterminal-symbol?
                 production-id production-precedence production-table
                 terminal-symbol? union-values)
        (only-in ./lr-automaton
                 transition-index transition-target)
        (only-in ./lr-lookahead build-states-via-lr0))
(export compile-lr-spec)

;; fork-action
;; : (-> List List List)
(def (fork-action left right)
  (let* ((left-actions (if (eq? (car left) 'fork) (cdr left) (list left)))
         (right-actions (if (eq? (car right) 'fork) (cdr right) (list right))))
    (cons 'fork (union-values left-actions right-actions))))

;; : (-> (Maybe List) Boolean)
(def (dynamic-precedence? precedence)
  (and precedence (eq? (car precedence) 'dynamic)))

;; : (-> Symbol List List String Datum List)
(def (fork-or-reject conflict-policy shift reduce message evidence)
  (if (eq? conflict-policy 'selective-glr)
    (fork-action shift reduce)
    (error message evidence)))

;; resolve-shift-reduce
;; : (-> Datum List List Vector Symbol List)
(def (resolve-shift-reduce terminal shift reduce table conflict-policy)
  (let* ((production (vector-ref table (cadr reduce)))
         (reduce-precedence (production-precedence production))
         (shift-precedence (caddr shift)))
    (cond
     ((or (dynamic-precedence? reduce-precedence)
          (dynamic-precedence? shift-precedence))
      (fork-or-reject
       conflict-policy shift reduce
       "dynamic precedence requires selective GLR admission" terminal))
     ((not (and reduce-precedence shift-precedence))
      (fork-or-reject
       conflict-policy shift reduce
       "unresolved shift/reduce conflict requires precedence"
       (list terminal production)))
     (else
      (let ((reduce-rank (cadr reduce-precedence))
            (shift-rank (cadr shift-precedence)))
        (cond
         ((> reduce-rank shift-rank) reduce)
         ((< reduce-rank shift-rank) shift)
         ((eq? (car reduce-precedence) 'left) reduce)
         ((eq? (car reduce-precedence) 'right) shift)
         ((eq? (car reduce-precedence) 'none)
          (list 'reject-nonassoc terminal reduce-rank))
         (else
          (error "unknown static associativity"
                 terminal reduce-precedence))))))))

;; resolve-reduce-reduce
;; : (-> Datum List List Vector Symbol List)
(def (resolve-reduce-reduce terminal left right table conflict-policy)
  (let* ((left-production (vector-ref table (cadr left)))
         (right-production (vector-ref table (cadr right)))
         (left-precedence (production-precedence left-production))
         (right-precedence (production-precedence right-production))
         (left-rank (if left-precedence (cadr left-precedence) 0))
         (right-rank (if right-precedence (cadr right-precedence) 0)))
    (cond
     ((> left-rank right-rank) left)
     ((< left-rank right-rank) right)
     ((eq? conflict-policy 'selective-glr) (fork-action left right))
     (else
      (error "unresolved reduce/reduce conflict"
             terminal left-production right-production)))))

;; resolve-action
;; : (-> Fixnum Datum List List Vector Symbol List)
(def (resolve-action state terminal current action table conflict-policy)
  (cond
   ((equal? current action) current)
   ((or (eq? (car current) 'fork) (eq? (car action) 'fork))
    (fork-action current action))
   ((eq? (car current) 'reject-nonassoc) current)
   ((eq? (car action) 'reject-nonassoc) action)
   ((and (eq? (car current) 'shift)
         (eq? (car action) 'shift)
         (= (cadr current) (cadr action)))
    (let ((current-precedence (caddr current))
          (next-precedence (caddr action)))
      (cond
       ((and (not current-precedence) (not next-precedence)) current)
       ((not current-precedence) action)
       ((not next-precedence) current)
       ((>= (cadr current-precedence) (cadr next-precedence)) current)
       (else action))))
   ((and (eq? (car current) 'shift) (eq? (car action) 'reduce))
    (resolve-shift-reduce terminal current action table conflict-policy))
   ((and (eq? (car current) 'reduce) (eq? (car action) 'shift))
    (resolve-shift-reduce terminal action current table conflict-policy))
   ((and (eq? (car current) 'reduce) (eq? (car action) 'reduce))
    (resolve-reduce-reduce terminal current action table conflict-policy))
   (else
    (error "unresolved LR action conflict" state terminal current action))))

;; index-state-rows
;; : (-> List Fixnum Procedure Procedure Vector)
(def (index-state-rows rows state-count key-procedure value-procedure)
  (let (indexed (make-vector state-count '()))
    (for-each
     (lambda (row)
       (let (state (car row))
         (vector-set! indexed state
                      (cons (cons (key-procedure row) (value-procedure row))
                            (vector-ref indexed state)))))
     rows)
    (let loop ((state 0))
      (when (< state state-count)
        (vector-set! indexed state (reverse (vector-ref indexed state)))
        (loop (+ state 1))))
    indexed))

;; build-actions
;; : (-> Vector Fixnum Vector Vector List Vector Vector Pair Vector Symbol
;;        (values Vector Fixnum))
(def (build-actions states state-count lookaheads lookahead-offsets transitions table
                    terminal-values layout core-symbols conflict-policy)
  (let ((actions (make-vector state-count '()))
        (transitions (transition-index transitions))
        (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1"))
        (started (##current-time-point))
        (processed-items 0)
        (published-actions 0)
        (publication-count 0))
    ;; Publish exactly one action row per state. Keeping publication outside the
    ;; item traversal avoids rebuilding the growing row on recursive returns.
    (for (state-id (in-range state-count))
      (let (state-items (vector-ref states state-id))
       (unless (list? state-items)
         (error "LR action state is not an item list"
                 state-id state-items))
       (let ((state-actions (make-table test: equal?))
             (terminal-order '())
             (lookahead-node (vector-ref lookahead-offsets state-id)))
         (def (install! terminal action)
           (let (current (table-ref state-actions terminal #f))
             (if current
               (table-set! state-actions terminal
                           (resolve-action state-id terminal current action
                                           table conflict-policy))
               (begin
                 (table-set! state-actions terminal action)
                 (set! terminal-order (cons terminal terminal-order))))))
         (for-each
          (lambda (item)
            (set! processed-items (+ processed-items 1))
            (let* ((production-id (quotient item (cdr layout)))
                   (symbol (vector-ref core-symbols item)))
              (cond
               ((and symbol (terminal-symbol? symbol))
                (install!
                 symbol
                 (list 'shift
                       (transition-target transitions state-id symbol)
                       (production-precedence
                        (vector-ref table production-id)))))
               ((not symbol)
                (let (action
                      (if (= production-id 0)
                        '(accept)
                        (list 'reduce production-id)))
                  (compiler-index-set-for-each
                   (vector-ref lookaheads lookahead-node)
                   (lambda (lookahead)
                     (install! (vector-ref terminal-values lookahead)
                               action)))))))
            (set! lookahead-node (+ lookahead-node 1)))
          state-items)
         (vector-set!
          actions state-id
          (map (lambda (terminal)
                 (cons terminal (table-ref state-actions terminal)))
               (reverse terminal-order)))
         (set! publication-count (+ publication-count 1))
         (set! published-actions
               (+ published-actions (length terminal-order))))
       (when (and trace? (zero? (modulo (+ state-id 1) 100)))
         (display "[gerbil-parser-lr] action-states=")
         (display (+ state-id 1))
         (display " items=")
         (display processed-items)
         (display " actions=")
         (display published-actions)
         (display " elapsed-ms=")
         (displayln
          (inexact->exact
           (floor (* 1000.0 (- (##current-time-point) started)))))
         (force-output))))
    (values actions publication-count)))

;; build-gotos
;; : (-> List Fixnum Vector)
(def (build-gotos transitions state-count)
  (index-state-rows
   (filter (lambda (row) (nonterminal-symbol? (cadr row))) transitions)
   state-count
   (lambda (row) (nonterminal-name (cadr row)))
   caddr))

;; trace-lr-phase
;; : (-> Symbol Fixnum Flonum Void)
(def (trace-lr-phase phase count started)
  (when (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")
    (display "[gerbil-parser-lr] phase=")
    (display phase)
    (display " count=")
    (display count)
    (display " elapsed-ms=")
    (displayln (inexact->exact
                (floor (* 1000.0 (- (##current-time-point) started)))))
    (force-output)))

;;; Linearizes lowering, fixed-point analysis, state construction, conflict
;;; resolution, and publication into the canonical immutable LR spec v1.
;; compile-lr-spec
;; : (-> List Symbol Symbol Boolean List)
(def (compile-lr-spec rules root (conflict-policy 'reject)
                      (case-insensitive? #f))
  (let* ((started (##current-time-point))
         (productions
          (lower-rules rules root (eq? conflict-policy 'selective-glr)))
         (table (production-table productions)))
    (trace-lr-phase 'lowered (length productions) started)
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (trace-lr-phase 'nullable (length nullable) started)
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (trace-lr-phase 'first (length first) started)
        (let-values (((states state-count lookaheads lookahead-offsets transitions
                              terminal-values layout core-symbols
                             lr0-state-visit-count lookahead-item-visit-count)
                      (build-states-via-lr0
                       productions table first-index nullable-index)))
          (trace-lr-phase 'states state-count started)
          (let-values (((actions action-state-publication-count)
                        (build-actions
                         states state-count lookaheads lookahead-offsets transitions
                         table terminal-values layout core-symbols
                         conflict-policy)))
            (unless (= action-state-publication-count state-count)
              (error "LR action rows were not published exactly once per state"
                     state-count action-state-publication-count))
            (trace-lr-phase 'actions (vector-length actions) started)
            (let (gotos (build-gotos transitions state-count))
              (trace-lr-phase 'gotos (vector-length gotos) started)
              (list
               (cons 'schema "gerbil-parser.lr-spec.v1")
               (cons 'case-insensitive? case-insensitive?)
               (cons 'productions productions)
               (cons 'nullable nullable)
               (cons 'first first)
               (cons 'algorithm 'lalr1-lr0-fixed-point-v1)
               (cons 'state-count state-count)
               (cons 'lr0-state-visit-count lr0-state-visit-count)
               (cons 'lookahead-item-visit-count lookahead-item-visit-count)
               (cons 'actions actions)
               (cons 'gotos gotos)))))))))
