;;; -*- Gerbil -*-
;;; LR conflict resolution and canonical parser-table publication.

(import (only-in :std/iter for in-range)
        (only-in ./funcs compiler-index-set-for-each)
        (only-in ./lr
                 compute-first compute-nullable
                 lower-rules lr-spec-ref nonterminal-name nonterminal-symbol?
                 production-action production-id production-precedence production-table
                 terminal-symbol? union-values)
        (only-in ./lr-lookahead
                 build-states-via-lr0 build-states-via-canonical-lr1)
        (only-in ./lr-partition build-states-via-partitioned-lr1)
        (only-in ./lr-follow-automaton
                 build-states-via-follow-partition-lr1))
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
  (if (memq conflict-policy '(selective-glr probe))
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
     ((or (dynamic-precedence? left-precedence)
          (dynamic-precedence? right-precedence))
      (fork-or-reject
       conflict-policy left right
       "dynamic precedence requires selective GLR admission"
       (list terminal left-production right-production)))
     ((> left-rank right-rank) left)
     ((< left-rank right-rank) right)
     ((memq conflict-policy '(selective-glr probe))
      (fork-action left right))
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
        (transition-rows
         (index-state-rows transitions state-count cadr caddr))
        (terminal-index (make-table test: equal?))
        (state-actions (make-vector (vector-length terminal-values) #f))
        (raw-reductions (make-vector (vector-length terminal-values) '()))
        (shift-targets (make-vector (vector-length terminal-values) #f))
        (core-terminal-ids (make-vector (vector-length core-symbols) #f))
        (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1"))
        (started (##current-time-point))
        (layout? (vector-any (lambda (production)
                     (let (action (production-action production))
                       (or (eq? action 'layout-end)
                           (and (pair? action) (eq? (car action) 'layout-end))))) table))
        (processed-items 0)
        (published-actions 0)
        (publication-count 0))
    (let index-terminals ((id 0))
      (when (< id (vector-length terminal-values))
        (table-set! terminal-index (vector-ref terminal-values id) id)
        (index-terminals (+ id 1))))
    (let index-cores ((item 0))
      (when (< item (vector-length core-symbols))
        (let (symbol (vector-ref core-symbols item))
          (when (and symbol (terminal-symbol? symbol))
            (let (id (table-ref terminal-index symbol #f))
              (unless id (error "LR terminal is not indexed" item symbol))
              (vector-set! core-terminal-ids item id))))
        (index-cores (+ item 1))))
    ;; Publish exactly one action row per state. Keeping publication outside the
    ;; item traversal avoids rebuilding the growing row on recursive returns.
    (for (state-id (in-range state-count))
      (let (state-items (vector-ref states state-id))
       (unless (list? state-items)
         (error "LR action state is not an item list"
                 state-id state-items))
       (let ((terminal-order '())
             (shift-order '())
             (lookahead-node (vector-ref lookahead-offsets state-id)))
         (def (install! terminal-id action)
           (when (and layout? (eq? (car action) 'reduce))
             (vector-set! raw-reductions terminal-id
              (cons action (vector-ref raw-reductions terminal-id))))
           (let (current (vector-ref state-actions terminal-id))
             (if current
               (vector-set! state-actions terminal-id
                           (resolve-action state-id (vector-ref terminal-values terminal-id)
                                           current action
                                           table conflict-policy))
               (begin
                 (vector-set! state-actions terminal-id action)
                 (set! terminal-order (cons terminal-id terminal-order))))))
         (for-each
          (lambda (row)
            (when (terminal-symbol? (car row))
              (let (id (table-ref terminal-index (car row) #f))
                (unless id (error "LR shift terminal is not indexed" state-id row))
                (vector-set! shift-targets id (cdr row))
                (set! shift-order (cons id shift-order)))))
          (vector-ref transition-rows state-id))
         (for-each
          (lambda (item)
            (set! processed-items (+ processed-items 1))
            (let* ((production-id (quotient item (cdr layout)))
                   (symbol (vector-ref core-symbols item))
                   (terminal-id (vector-ref core-terminal-ids item)))
              (cond
               (terminal-id
                (install!
                 terminal-id
                 (list 'shift
                       (let (target (vector-ref shift-targets terminal-id))
                         (unless target (error "LR shift transition is missing"
                                               state-id symbol))
                         target)
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
                     (install! lookahead action)))))))
            (set! lookahead-node (+ lookahead-node 1)))
          state-items)
         (vector-set!
          actions state-id
          (map (lambda (terminal-id)
                 (let ((action (vector-ref state-actions terminal-id))
                       (reductions (vector-ref raw-reductions terminal-id)))
                   (cons (vector-ref terminal-values terminal-id)
                    (if (and layout? (eq? (car action) 'shift) (pair? reductions))
                      (list 'layout-guard action
                       (if (null? (cdr reductions)) (car reductions)
                         (cons 'fork (reverse reductions)))) action))))
               (reverse terminal-order)))
         (set! publication-count (+ publication-count 1))
         (set! published-actions
               (+ published-actions (length terminal-order)))
         ;; Rows own copied immutable cells; clear only the touched scratch
         ;; slots instead of allocating state-count times the terminal domain.
         (for-each (lambda (id)
                     (vector-set! state-actions id #f)
                     (vector-set! raw-reductions id '())) terminal-order)
         (for-each (lambda (id) (vector-set! shift-targets id #f)) shift-order))
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
(def (compile-lr-spec/selected rules root conflict-policy
                               case-insensitive? construction)
  (unless (memq construction
                '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
    (error "unknown LR construction" construction))
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
                      ((case construction
                         ((canonical-lr1) build-states-via-canonical-lr1)
                         ((partitioned-lr1) build-states-via-partitioned-lr1)
                         ((follow-partition-lr1)
                          build-states-via-follow-partition-lr1)
                         (else build-states-via-lr0))
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
              (let (spec
                    (list
                     (cons 'schema "gerbil-parser.lr-spec.v1")
                     (cons 'case-insensitive? case-insensitive?)
                     (cons 'productions productions)
                     (cons 'nullable nullable)
                     (cons 'first first)
                     (cons 'algorithm
                           (case construction
                             ((canonical-lr1) 'canonical-lr1-reference-v1)
                             ((partitioned-lr1)
                              'conflict-partitioned-lr1-v1)
                             ((follow-partition-lr1)
                              'follow-partition-lr1-v1)
                             (else 'lalr1-lr0-fixed-point-v1)))
                     (cons 'state-count state-count)
                     (cons 'lr0-state-visit-count
                           (and (eq? construction 'lalr)
                                lr0-state-visit-count))
                     (cons 'lookahead-item-visit-count
                           (and (eq? construction 'lalr)
                                lookahead-item-visit-count))
                     (cons 'actions actions)
                     (cons 'gotos gotos)))
                (case construction
                  ((lalr) spec)
                  ((follow-partition-lr1)
                   (cons (cons 'follow-block-count
                               lr0-state-visit-count)
                         (cons (cons 'output-item-count
                                     lookahead-item-visit-count)
                               spec)))
                  (else
                   (cons (cons 'canonical-state-count
                               lr0-state-visit-count)
                         (cons (cons 'output-item-count
                                     lookahead-item-visit-count)
                               spec))))))))))))

;;; The ordinary path stays LALR. Probe publishes unresolved cells as forks
;;; without admitting GLR lowering; only those cells trigger the LR(1) route.
;;; This avoids using exception construction as routine algorithm selection.
(def (lr-spec-has-fork? spec)
  (vector-any
   (lambda (row)
     (any (lambda (entry) (eq? (car (cdr entry)) 'fork)) row))
   (lr-spec-ref spec 'actions)))

(def (compile-lr-spec rules root (conflict-policy 'reject)
                      (case-insensitive? #f) (construction 'lalr))
  (if (eq? construction 'lalr-then-follow)
    (if (eq? conflict-policy 'reject)
      (let (probe
            (compile-lr-spec/selected
             rules root 'probe case-insensitive? 'lalr))
        (if (lr-spec-has-fork? probe)
          (compile-lr-spec/selected
           rules root conflict-policy case-insensitive?
           'follow-partition-lr1)
          probe))
      (compile-lr-spec/selected
       rules root conflict-policy case-insensitive? 'lalr))
    (compile-lr-spec/selected
     rules root conflict-policy case-insensitive? construction)))
