;;; -*- Gerbil -*-
;;; Deterministic LALR(1) compilation and lossless recognition runtime.

(import :std/sort
        ./funcs
        ../runtime/recognition
        ../runtime/reduce
        ../runtime/token)
(export compile-lr-spec
        lr-spec-ref
        lr-parse)

(def +lr-eof+ '(terminal eof))

(def (append-unique values value)
  (if (member value values) values (append values (list value))))

(def (union-values left right)
  (let loop ((rest right) (found left))
    (if (null? rest)
      found
      (loop (cdr rest) (append-unique found (car rest))))))

(def (alist-ref rows key (default #f))
  (let (entry (assq key rows))
    (if entry (cdr entry) default)))

(def (alist-set rows key value)
  (let loop ((rest rows) (found '()))
    (cond
     ((null? rest) (reverse (cons (cons key value) found)))
     ((eq? (caar rest) key)
      (append (reverse found) (cons (cons key value) (cdr rest))))
     (else (loop (cdr rest) (cons (car rest) found))))))

(def (terminal-symbol? value)
  (and (pair? value) (eq? (car value) 'terminal)))

(def (nonterminal-symbol? value)
  (and (pair? value) (eq? (car value) 'nonterminal)))

(def (nonterminal-name value) (cadr value))

(def (marked-symbol? value)
  (and (pair? value) (eq? (car value) 'marked)))

(def (base-symbol value)
  (if (marked-symbol? value) (cadr value) value))

(def (operand-actions value)
  (if (marked-symbol? value) (caddr value) '()))

(def (operand-add-action value action)
  (if (marked-symbol? value)
    (list 'marked (cadr value) (append (caddr value) (list action)))
    (list 'marked value (list action))))

(def (make-production id lhs rhs action precedence)
  (list id lhs rhs action precedence))
(def (production-id value) (car value))
(def (production-lhs value) (cadr value))
(def (production-rhs value) (caddr value))
(def (production-action value) (cadddr value))
(def (production-precedence value) (car (cddddr value)))

(def (lower-rules rules root)
  (let ((next-id 1)
        (next-synthetic 0)
        (productions '()))
    (def (emit lhs rhs action precedence)
      (let (production
            (make-production next-id lhs rhs action precedence))
        (set! next-id (+ next-id 1))
        (set! productions (cons production productions))))
    (def (fresh owner)
      (let (name
            (string->symbol
             (string-append "$" (symbol->string owner) "."
                            (number->string next-synthetic))))
        (set! next-synthetic (+ next-synthetic 1))
        name))
    (def (lower-symbol owner expression precedence)
      (case (car expression)
        ((literal) (list 'terminal 'literal (cadr expression)))
        ((token) (list 'terminal 'token (cadr expression)))
        ((reference) (list 'nonterminal (cadr expression)))
        (else
         (let (name (fresh owner))
           (lower-to name owner expression precedence)
           (list 'nonterminal name)))))
    (def (lower-operand owner expression precedence)
      (case (car expression)
        ((field)
         (operand-add-action
          (lower-operand owner (caddr expression) precedence)
          (list 'field (cadr expression))))
        ((alias)
         (operand-add-action
          (lower-operand owner (caddr expression) precedence)
          (list 'alias (cadr expression))))
        (else (lower-symbol owner expression precedence))))
    (def (lower-to lhs owner expression precedence)
      (case (car expression)
        ((precedence)
         (let ((direction (cadr expression))
               (rank (caddr expression)))
           (when (eq? direction 'dynamic)
             (error "dynamic precedence requires selective GLR admission"
                    owner rank))
           (lower-to lhs owner (cadddr expression)
                     (list direction rank))))
        ((choice)
         (for-each
          (lambda (child) (lower-to lhs owner child precedence))
          (cdr expression)))
        ((empty) (emit lhs '() 'concat precedence))
        ((sequence)
         (emit lhs
               (map (lambda (child)
                      (lower-operand owner child precedence))
                    (cdr expression))
               'concat precedence))
        ((optional)
         (emit lhs '() 'concat precedence)
         (emit lhs
               (list (lower-operand owner (cadr expression) precedence))
               'pass precedence))
        ((repeat)
         (emit lhs '() 'concat precedence)
         (emit lhs
               (list (list 'nonterminal lhs)
                     (lower-operand owner (cadr expression) precedence))
               'concat precedence))
        ((repeat1)
         (let (child (lower-operand owner (cadr expression) precedence))
           (emit lhs (list child) 'pass precedence)
           (emit lhs (list (list 'nonterminal lhs) child)
                 'concat precedence)))
        ((field)
         (emit lhs (list (lower-operand owner expression precedence))
               'pass precedence))
        ((alias)
         (emit lhs (list (lower-operand owner expression precedence))
               'pass precedence))
        ((literal token reference)
         (emit lhs (list (lower-symbol owner expression precedence))
               'pass precedence))
        (else (error "unsupported GrammarExpr in LR lowering" owner expression))))
    (for-each
     (lambda (row) (lower-to (car row) (car row) (cadr row) #f))
     rules)
    (cons (make-production 0 '$accept
                           (list (list 'nonterminal root)) 'pass #f)
          (reverse productions))))

(def (production-table productions)
  (list->vector productions))

(def (production-index-by-lhs productions)
  (let (index (make-table test: eq?))
    (for-each
     (lambda (production)
       (let* ((lhs (production-lhs production))
              (found (table-ref index lhs '())))
         (table-set! index lhs (cons production found))))
    (reverse productions))
    index))

(def (production-terminal-catalog productions)
  (let ((seen (make-table test: equal?)))
    (table-set! seen +lr-eof+ #t)
  (let loop-productions ((rest productions) (found (list +lr-eof+)))
    (if (null? rest)
      (let ((terminal-values (list->vector (reverse found)))
            (terminal-index (make-table test: equal?)))
        (let install ((offset 0))
          (when (< offset (vector-length terminal-values))
            (table-set! terminal-index
                        (vector-ref terminal-values offset) offset)
            (install (+ offset 1))))
        (values terminal-values terminal-index))
      (let loop-symbols ((symbols (production-rhs (car rest)))
                         (next found))
        (if (null? symbols)
          (loop-productions (cdr rest) next)
          (let (symbol (base-symbol (car symbols)))
            (loop-symbols
             (cdr symbols)
             (if (and (terminal-symbol? symbol)
                      (not (table-ref seen symbol #f)))
               (begin
                 (table-set! seen symbol #t)
                 (cons symbol next))
               next)))))))))

(def (nonterminals productions)
  (let (seen (make-table test: eq?))
  (let loop ((rest productions) (found '()))
    (if (null? rest)
      (reverse found)
      (let (name (production-lhs (car rest)))
        (if (table-ref seen name #f)
          (loop (cdr rest) found)
          (begin
            (table-set! seen name #t)
            (loop (cdr rest) (cons name found)))))))))

(def (symbol-nullable? symbol nullable)
  (let (symbol (base-symbol symbol))
    (and (nonterminal-symbol? symbol)
         (table-ref nullable (nonterminal-name symbol) #f))))

(def (rhs-nullable? rhs nullable)
  (let loop ((rest rhs))
    (or (null? rest)
        (and (symbol-nullable? (car rest) nullable)
             (loop (cdr rest))))))

(def (compute-nullable productions)
  (let ((names (nonterminals productions))
        (nullable (make-table test: eq?)))
    (let loop ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (let (lhs (production-lhs production))
             (when (and (not (table-ref nullable lhs #f))
                        (rhs-nullable? (production-rhs production) nullable))
               (table-set! nullable lhs #t)
               (set! changed? #t))))
         productions)
        (if changed?
          (loop)
          (values (filter (lambda (name) (table-ref nullable name #f)) names)
                  nullable))))))

(def (symbol-first symbol first)
  (let (symbol (base-symbol symbol))
    (if (terminal-symbol? symbol)
      (list symbol)
      (table-ref first (nonterminal-name symbol) '()))))

(def (sequence-first symbols first nullable (tail-lookahead #f))
  (let loop ((rest symbols) (found '()))
    (cond
     ((null? rest)
      (if tail-lookahead (append-unique found tail-lookahead) found))
     (else
      (let ((symbol (car rest)))
        (let (next (union-values found (symbol-first symbol first)))
          (if (symbol-nullable? symbol nullable)
            (loop (cdr rest) next)
            next)))))))

(def (compute-first productions nullable)
  (let ((names (nonterminals productions))
        (first (make-table test: eq?)))
    (for-each (lambda (name) (table-set! first name '())) names)
    (let loop ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (let* ((lhs (production-lhs production))
                  (before (table-ref first lhs '()))
                  (after
                   (union-values
                    before
                    (sequence-first (production-rhs production)
                                    first nullable))))
             (unless (equal? before after)
               (table-set! first lhs after)
               (set! changed? #t))))
         productions)
        (if changed?
          (loop)
          (values
           (map (lambda (name) (cons name (table-ref first name '()))) names)
           first))))))

(def (make-item-layout table terminal-values)
  (let loop ((index 0) (dot-width 1))
    (if (= index (vector-length table))
      (cons (vector-length terminal-values) dot-width)
      (loop (+ index 1)
            (max dot-width
                 (+ (length (production-rhs (vector-ref table index))) 1))))))

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
  (let loop ((rest rows) (found '()))
    (cond
     ((null? rest)
      (reverse (cons (cons symbol target) found)))
     ((equal? (caar rest) symbol)
      (append (reverse found)
              (cons (cons symbol target) (cdr rest))))
     (else (loop (cdr rest) (cons (car rest) found))))))

(def (materialize-transitions state-transitions state-count)
  (let state-loop ((state 0) (found '()))
    (if (= state state-count)
      (reverse found)
      (let row-loop ((rows (vector-ref state-transitions state))
                     (found found))
        (if (null? rows)
          (state-loop (+ state 1) found)
          (row-loop (cdr rows)
                    (cons (list state (caar rows) (cdar rows)) found)))))))

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

(def (core-item-production-id item layout)
  (quotient item (cdr layout)))

(def (core-item-dot item layout)
  (modulo item (cdr layout)))

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

(def (lr0-closure seed productions-by-lhs table layout)
  (let (seen (make-table test: eq?))
    (for-each (lambda (item) (table-set! seen item #t)) seed)
    (let loop ((pending seed) (items '()))
      (if (null? pending)
        (sort (reverse items) <)
        (let* ((item (car pending))
               (symbol (core-item-after-dot item layout table))
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

(def (lr0-state-kernels state layout table)
  (let ((kernels (make-table test: equal?))
        (symbol-order '()))
    (for-each
     (lambda (item)
       (let (symbol (core-item-after-dot item layout table))
         (when symbol
           (unless (table-ref kernels symbol #f)
             (set! symbol-order (cons symbol symbol-order)))
           (table-set! kernels symbol
                       (cons (+ item 1)
                             (or (table-ref kernels symbol #f) '()))))))
     state)
    (map (lambda (symbol) (cons symbol (table-ref kernels symbol)))
         (reverse symbol-order))))

(def (build-lr0-automaton productions table layout)
  (let* ((productions-by-lhs (production-index-by-lhs productions))
         (initial (lr0-closure (list (make-core-item 0 0 layout))
                               productions-by-lhs table layout))
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
    (let loop ((queue (cons '(0) '())))
      (if (compiler-work-queue-empty? queue)
        (values states state-transitions state-count productions-by-lhs
                processed-count)
        (let-values (((index next-queue) (compiler-work-queue-pop queue)))
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
                                         productions-by-lhs table layout))
                    (existing (table-ref state-index target #f))
                    (target-index (or existing (append-state! target))))
               (vector-set!
                state-transitions index
                (transition-set (vector-ref state-transitions index)
                                symbol target-index))
               (unless existing
                 (set! next-queue
                       (compiler-work-queue-push-back
                        next-queue target-index)))))
           (lr0-state-kernels (vector-ref states index) layout table))
          (loop next-queue))))))

(def (lookahead-mask->list mask)
  (let ((found '()))
    (compiler-terminal-set-for-each mask
      (lambda (lookahead) (set! found (cons lookahead found))))
    (reverse found)))

(def (propagate-lalr-lookaheads states state-transitions state-count
                                productions-by-lhs table first nullable
                                terminal-values terminal-index layout)
  (let ((lookaheads (make-table test: eq?))
        (pending (make-table test: eq?))
        (queued (make-table test: eq?))
        (queue (cons '() '()))
        (item-space (* (cdr layout) (vector-length table)))
        (processed-count 0)
        (trace? (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")))
    (def (item-key state item)
      (+ item (* item-space state)))
    (def (enqueue! state item lookahead)
      (let* ((key (item-key state item))
             (known (table-ref lookaheads key 0)))
        (unless (compiler-terminal-set-member? known lookahead)
          (table-set! lookaheads key
                      (compiler-terminal-set-add known lookahead))
          (table-set! pending key
                      (compiler-terminal-set-add
                       (table-ref pending key 0) lookahead))
          (unless (table-ref queued key #f)
            (table-set! queued key #t)
            (set! queue (compiler-work-queue-push-back queue key))))))
    (enqueue! 0 (make-core-item 0 0 layout)
              (table-ref terminal-index +lr-eof+))
    (let loop ()
      (unless (compiler-work-queue-empty? queue)
        (let-values (((key next-queue) (compiler-work-queue-pop queue)))
          (set! queue next-queue)
          (table-set! queued key #f)
          (let* ((state (quotient key item-space))
                 (item (modulo key item-space))
                 (delta (table-ref pending key 0))
                 (symbol (core-item-after-dot item layout table)))
            (table-set! pending key 0)
            (set! processed-count (+ processed-count 1))
            (when (and trace? (zero? (modulo processed-count 10000)))
              (display "[gerbil-parser-lr] lookahead-items=")
              (displayln processed-count)
              (force-output))
            (when (and symbol (nonterminal-symbol? symbol))
              (let (tail (core-item-tail-after-next item layout table))
                (let production-loop
                    ((productions
                      (table-ref productions-by-lhs
                                 (nonterminal-name symbol) '())))
                  (unless (null? productions)
                    (let (target-item
                          (make-core-item
                           (production-id (car productions)) 0 layout))
                      (compiler-terminal-set-for-each
                       delta
                       (lambda (lookahead)
                         (for-each
                          (lambda (terminal)
                            (enqueue!
                             state target-item
                             (table-ref terminal-index terminal)))
                          (sequence-first
                           tail first nullable
                           (vector-ref terminal-values lookahead))))))
                    (production-loop (cdr productions))))))
            (when symbol
              (let (transition
                    (assoc symbol (vector-ref state-transitions state)))
                (unless transition
                  (error "LR(0) transition missing during lookahead propagation"
                         state item symbol))
                (compiler-terminal-set-for-each
                 delta
                 (lambda (lookahead)
                   (enqueue! (cdr transition) (+ item 1) lookahead))))))
          (loop))))
    (values
     (let materialize-state ((state 0) (found '()))
       (if (= state state-count)
         (reverse found)
         (let item-loop ((items (vector-ref states state))
                         (state-items '()))
           (if (null? items)
             (materialize-state (+ state 1)
                                (cons (sort state-items item<?) found))
             (let* ((item (car items))
                    (item-lookaheads
                     (lookahead-mask->list
                      (table-ref lookaheads (item-key state item) 0))))
               (item-loop
                (cdr items)
                (append
                 (map (lambda (lookahead)
                        (+ lookahead (* (car layout) item)))
                      item-lookaheads)
                 state-items)))))))
     processed-count)))

(def (build-states-via-lr0 productions table first nullable)
  (let-values (((terminal-values terminal-index)
                (production-terminal-catalog productions)))
    (let (layout (make-item-layout table terminal-values))
      (let-values (((states state-transitions state-count productions-by-lhs
                            lr0-state-visit-count)
                    (build-lr0-automaton productions table layout)))
        (let-values (((materialized-states lookahead-item-visit-count)
                      (propagate-lalr-lookaheads
                       states state-transitions state-count productions-by-lhs
                       table first nullable terminal-values terminal-index
                       layout)))
          (values materialized-states
                  (materialize-transitions state-transitions state-count)
                  terminal-values layout
                  lr0-state-visit-count lookahead-item-visit-count))))))

(def (fork-action left right)
  (let* ((left-actions (if (eq? (car left) 'fork) (cdr left) (list left)))
         (right-actions (if (eq? (car right) 'fork) (cdr right) (list right))))
    (cons 'fork (union-values left-actions right-actions))))

(def (resolve-shift-reduce terminal shift reduce table conflict-policy)
  (let* ((production (vector-ref table (cadr reduce)))
         (reduce-precedence (production-precedence production))
         (shift-precedence (caddr shift)))
    (if (not (and reduce-precedence shift-precedence))
      (if (eq? conflict-policy 'selective-glr)
        (fork-action shift reduce)
        (error "unresolved shift/reduce conflict requires precedence"
               terminal shift production))
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
                 terminal reduce-precedence)))))))

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

(def (index-state-rows rows state-count key-procedure value-procedure)
  (let (indexed (make-vector state-count '()))
    (for-each
     (lambda (row)
       (let (state (car row))
         (vector-set!
          indexed state
          (cons (cons (key-procedure row) (value-procedure row))
                (vector-ref indexed state)))))
     rows)
    (let loop ((state 0))
      (when (< state state-count)
        (vector-set! indexed state (reverse (vector-ref indexed state)))
        (loop (+ state 1))))
    indexed))

(def (build-actions states transitions productions table terminal-values layout
                    conflict-policy)
  (let ((actions (make-vector (length states) '()))
        (transitions (transition-index transitions)))
    (let state-loop ((rest states) (state-id 0))
      (unless (null? rest)
        (unless (list? (car rest))
          (error "LR action state is not an item list"
                 state-id (car rest)))
        (let ((state-actions (make-table test: equal?))
              (terminal-order '()))
        (def (install! terminal action)
          (let (current (table-ref state-actions terminal #f))
            (if current
              (table-set!
               state-actions terminal
               (resolve-action state-id terminal current action
                               table conflict-policy))
              (begin
                (table-set! state-actions terminal action)
                (set! terminal-order (cons terminal terminal-order))))))
        (let item-loop ((items (car rest)))
          (unless (null? items)
            (let* ((item (car items))
                   (symbol (item-after-dot item layout table)))
              (cond
               ((and symbol (terminal-symbol? symbol))
                (install!
                 symbol
                 (list 'shift
                       (transition-target transitions state-id symbol)
                       (production-precedence
                        (vector-ref table (item-production-id item layout))))))
               ((item-complete? item layout table)
                (let (action
                      (if (= (item-production-id item layout) 0)
                        '(accept)
                        (list 'reduce (item-production-id item layout))))
                  (install!
                   (vector-ref terminal-values (item-lookahead item layout))
                   action))))
            (item-loop (cdr items))))
        (vector-set!
         actions state-id
         (map (lambda (terminal)
                (cons terminal (table-ref state-actions terminal)))
              (reverse terminal-order))))
        (state-loop (cdr rest) (+ state-id 1))))
    actions)))

(def (build-gotos transitions state-count)
  (index-state-rows
   (filter (lambda (row) (nonterminal-symbol? (cadr row))) transitions)
   state-count
   (lambda (row) (nonterminal-name (cadr row)))
   caddr))

(def (trace-lr-phase phase count)
  (when (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1")
    (display "[gerbil-parser-lr] phase=")
    (display phase)
    (display " count=")
    (displayln count)
    (force-output)))

(def (compile-lr-spec rules root (conflict-policy 'reject)
                      (case-insensitive? #f))
  (let* ((productions (lower-rules rules root))
         (table (production-table productions)))
    (trace-lr-phase 'lowered (length productions))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
    (trace-lr-phase 'nullable (length nullable))
    (let-values (((first first-index)
                  (compute-first productions nullable-index)))
    (trace-lr-phase 'first (length first))
    (let-values (((states transitions terminal-values layout
                          lr0-state-visit-count lookahead-item-visit-count)
                  (build-states-via-lr0
                   productions table first-index nullable-index)))
      (trace-lr-phase 'states (length states))
      (list
       (cons 'schema "gerbil-parser.lr-spec.v1")
       (cons 'case-insensitive? case-insensitive?)
       (cons 'productions productions)
       (cons 'nullable nullable)
       (cons 'first first)
       (cons 'algorithm 'lalr1-lr0-fixed-point-v1)
       (cons 'state-count (length states))
       (cons 'lr0-state-visit-count lr0-state-visit-count)
       (cons 'lookahead-item-visit-count lookahead-item-visit-count)
       (cons 'actions
             (build-actions states transitions productions table
                            terminal-values layout conflict-policy))
       (cons 'gotos (build-gotos transitions (length states)))))))))

(def (lr-spec-ref spec key)
  (alist-ref spec key))

(def (lookup-action actions state terminal)
  (let loop ((rest (vector-ref actions state)))
    (and (pair? rest)
         (let (row (car rest))
           (if (equal? (car row) terminal)
             (cdr row)
             (loop (cdr rest)))))))

(def (current-action actions state tokens case-insensitive?)
  (if (null? tokens)
    (lookup-action actions state +lr-eof+)
    ;; A literal is a contextual keyword/punctuation refinement of its lexical
    ;; token kind.  It therefore has deterministic priority over the generic
    ;; kind action; consulting both as peers creates a false LR conflict.
    (or (lookup-action actions state
                       (list 'terminal 'literal
                             (token-lexeme (car tokens))))
        (and case-insensitive?
             (string? (token-lexeme (car tokens)))
             (lookup-action actions state
                            (list 'terminal 'literal
                                  (string-upcase
                                   (token-lexeme (car tokens))))))
        (lookup-action actions state
                       (list 'terminal 'token
                             (token-kind (car tokens)))))))

(def (take values count)
  (if (zero? count) '()
      (cons (car values) (take (cdr values) (- count 1)))))

(def (drop values count)
  (if (zero? count) values (drop (cdr values) (- count 1))))

(def (apply-operand-actions value actions default-offset)
  (let loop ((rest actions) (children value))
    (if (null? rest)
      children
      (let (action (car rest))
        (loop
         (cdr rest)
         (case (car action)
           ((field)
            (recognition-children-field
             (cadr action) children default-offset))
           ((alias)
            (recognition-children-alias
             (cadr action) children default-offset))
           (else (error "unknown LR operand action" action))))))))

(def (reduce-value production reversed-values default-offset)
  (let* ((rhs (production-rhs production))
         (source-values (reverse reversed-values))
         (reduced-values
          (map (lambda (operand value)
                 (apply-operand-actions
                  value (operand-actions operand) default-offset))
               rhs source-values))
         (children (apply append reduced-values))
         (action (production-action production)))
    (cond
     ((eq? action 'concat) children)
     ((eq? action 'pass)
      (if (= (length reduced-values) 1)
        (car reduced-values)
        children))
     (else (error "unknown LR semantic action" action)))))

(def (goto-target gotos state lhs)
  (let (row (assq lhs (vector-ref gotos state)))
    (and row (cdr row))))

(def (lr-parse spec tokens)
  (let* ((productions (lr-spec-ref spec 'productions))
         (table (production-table productions))
         (actions (lr-spec-ref spec 'actions))
         (gotos (lr-spec-ref spec 'gotos))
         (case-insensitive? (lr-spec-ref spec 'case-insensitive?)))
    (def (try-action action states semantic-values rest)
      (case (car action)
        ((shift)
         (and (pair? rest)
              (try-parse
               (cons (cadr action) states)
               (cons (list (make-recognition-child #f (car rest)))
                     semantic-values)
               (cdr rest))))
        ((reduce)
         (let* ((production (vector-ref table (cadr action)))
                (count (length (production-rhs production)))
                (popped-values (take semantic-values count))
                (remaining-values (drop semantic-values count))
                (remaining-states (drop states count))
                (offset (if (pair? rest) (token-start (car rest))
                            (if (pair? tokens)
                              (token-end (car (reverse tokens))) 0)))
                (value (reduce-value production popped-values offset))
                (target
                 (and (pair? remaining-states)
                      (goto-target gotos (car remaining-states)
                                   (production-lhs production)))))
           (and target
                (try-parse (cons target remaining-states)
                           (cons value remaining-values) rest))))
        ((accept)
         (and (pair? semantic-values)
              (let (children (car semantic-values))
                (and (pair? children)
                     (null? (cdr children))
                     (not (recognition-child-field (car children)))
                     (list (recognition-child-value (car children)) rest)))))
        ((fork)
         (let loop ((branches (cdr action)))
           (and (pair? branches)
                (or (with-catch
                     (lambda (_) #f)
                     (lambda ()
                       (try-action (car branches)
                                   states semantic-values rest)))
                    (loop (cdr branches))))))
        ((reject-nonassoc)
         (error "non-associative operator cannot be chained"
                (cadr action) (caddr action)))
        (else (error "unknown LR action" action))))
    (def (try-parse states semantic-values rest)
      (let* ((state (car states))
             (action
              (current-action actions state rest case-insensitive?)))
        (and action (try-action action states semantic-values rest))))
    (let (result (try-parse '(0) '() tokens))
      (unless result
        (error "input does not match LR parser"
               (and (pair? tokens) (token-lexeme (car tokens)))))
      (values (car result) (cadr result)))))
