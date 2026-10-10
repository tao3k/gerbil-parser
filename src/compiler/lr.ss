;;; -*- Gerbil -*-
;;; Deterministic LALR(1) compilation and lossless recognition runtime.

(import (only-in ./funcs
                 compiler-index-set->ordered-values
                 compiler-index-set-difference
                 compiler-index-set-singleton
                 compiler-index-set-union))
(export +lr-eof+
        compute-first
        compute-nullable compute-completion
        compute-productive validate-resolved-start
        validate-resolved-repetitions current-grammar-source-map
        lr-spec-ref
        lower-rules
        base-symbol
        nonterminal-name
        nonterminal-symbol?
        operand-actions
        operand-actions-valid?
        layout-end-action? validate-production-semantics
        production-action
        production-id
        production-lhs
        production-precedence
        production-rhs
        production-table
        production-index-by-lhs
        production-terminal-catalog
        sequence-nullable?
        sequence-first
        terminal-symbol?
        union-values)

(def +lr-eof+ '(terminal eof))

(def (append-unique values value)
  (if (member value values)
      values
      (foldr cons (list value) values)))

;; union-values
;;   : (forall (a) (-> [a] [a] [a]))
;;   : (-> List List List)
;;   | doc m%
;;       `union-values` preserves first-observed order while removing duplicates.
;;
;;       # Examples
;;
;;       ```scheme
;;       (union-values '(a b) '(b c))
;;       ;; => (a b c)
;;       ```
;;     %
(def (union-values left right)
  (foldl (lambda (value found) (append-unique found value)) left right))

(def (alist-ref rows key (default #f))
  (let (entry (assq key rows))
    (if entry (cdr entry) default)))

(def (alist-set rows key value)
  (let (updated? #f)
    (let (next
          (map (lambda (entry)
                 (if (eq? (car entry) key)
                   (begin
                     (set! updated? #t)
                     (cons key value))
                   entry))
               rows))
      (if updated? next (foldr cons (list (cons key value)) rows)))))

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

;;; Canonical semantic names have one contract for prepared and generated
;;; execution. Validate a proper chain before either owner publishes a product.
(def (operand-actions-valid? actions)
  (and (list? actions)
       (every (lambda (action)
                (match action
                  (['field (? symbol?)] #t)
                  (['alias (? symbol?)] #t)
                  (_ #f)))
              actions)))

(def (operand-add-action value action)
  (if (marked-symbol? value)
    (list 'marked (cadr value) (append (caddr value) (list action)))
    (list 'marked value (list action))))

;;; Validate the producer's closed algebra before indexing or selecting an
;;; executor. These checks belong to canonical IR, not a backend policy.
(def (layout-end-action? action)
  ;; Classification after admission is constant work in the request path.
  (or (eq? action 'layout-end)
      (and (pair? action) (eq? (car action) 'layout-end))))

(def (canonical-layout-end-action? action)
  (or (eq? action 'layout-end)
      (and (list? action) (pair? action) (pair? (cdr action))
           (eq? (car action) 'layout-end)
           (every (lambda (boundary)
                    (and (string? boundary) (positive? (string-length boundary))))
                  (cdr action)))))

(def (canonical-base-symbol? value)
  (match value
    (['nonterminal (? symbol?)] #t)
    (['terminal 'token (? symbol?)] #t)
    (['terminal (or 'literal 'layout-start 'layout-next) (? string? text)]
     (positive? (string-length text)))
    (_ #f)))

(def (validate-canonical-operand operand)
  (match operand
    (['marked base actions]
     (unless (canonical-base-symbol? base)
       (error "invalid canonical LR operand" operand))
     (unless (operand-actions-valid? actions)
       (error "invalid LR operand actions" actions)))
    (_ (unless (canonical-base-symbol? operand)
         (error "invalid canonical LR operand" operand)))))

(def (validate-production-semantics productions)
  (unless (list? productions)
    (error "invalid canonical LR production list" productions))
  (let loop ((rest productions) (index 0))
    (unless (null? rest)
      (let (production (car rest))
        (unless
            (match production
              ([id (? symbol?) rhs action precedence]
               (and (fixnum? id) (fx= id index) (list? rhs)
                    (or (not precedence)
                        (match precedence
                          ([(or 'none 'left 'right 'dynamic) (? integer?)] #t)
                          (_ #f)))
                    (cond
                     ((eq? action 'pass) (and (pair? rhs) (null? (cdr rhs))))
                     ((eq? action 'concat) #t)
                     ((canonical-layout-end-action? action) (null? rhs))
                     (else #f))))
              (_ #f))
          (error "invalid canonical LR production" index production))
        (for-each validate-canonical-operand (production-rhs production)))
      (loop (cdr rest) (fx+ index 1))))
  productions)

(def (make-production id lhs rhs action precedence)
  (list id lhs rhs action precedence))
;; production-id
;;   : (forall (a) (-> [a] a))
;;   : (-> List Fixnum)
;;   | doc m%
;;       `production-id` returns the immutable production-table offset.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-id production)
;;       ;; => stable production table offset
;;       ```
;;     %
(def (production-id value) (car value))
;; production-lhs
;;   : (forall (a) (-> [a] a))
;;   : (-> List Symbol)
;;   | doc m%
;;       `production-lhs` returns the production's nonterminal owner.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-lhs production)
;;       ;; => declared nonterminal owner
;;       ```
;;     %
(def (production-lhs value) (cadr value))
;; production-rhs
;;   : (forall (a) (-> [a] [a]))
;;   : (-> List List)
;;   | doc m%
;;       `production-rhs` returns operands in declaration order.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-rhs production)
;;       ;; => source-ordered operands
;;       ```
;;     %
(def (production-rhs value) (caddr value))
;; production-action
;;   : (forall (a) (-> [a] a))
;;   : (-> List Symbol)
;;   | doc m%
;;       `production-action` returns the canonical reduction action.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-action production)
;;       ;; => canonical reduction action
;;       ```
;;     %
(def (production-action value) (cadddr value))
;; production-precedence
;;   : (forall (a) (-> [a] (Maybe a)))
;;   : (-> List (Maybe Pair))
;;   | doc m%
;;       `production-precedence` projects the optional static conflict policy.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-precedence production)
;;       ;; => precedence pair or #f
;;       ```
;;     %
(def (production-precedence value) (car (cddddr value)))

;;; Lowers grammar algebra into one indexed production table with the augmented root first.
;;; Rule order and operand actions are preserved because they determine conflict resolution.
;; lower-rules
;;   : (forall (r) (-> [(Pair Symbol r)] Symbol [List]))
;;   : (-> List Symbol List)
;;   | doc m%
;;       `lower-rules` preserves declaration order while assigning production ids.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lower-rules rules 'source-file)
;;       ;; => augmented production zero followed by ordered lowered productions
;;       ```
;;     %
(def (lower-rules rules root (allow-dynamic? #f) (observe-origin #f))
  (let ((next-id 1)
        (next-synthetic 0)
        (productions '()))
    (def (emit-production lhs rhs action precedence owner path expression)
      (let (production
            (make-production next-id lhs rhs action precedence))
        (when observe-origin
          (observe-origin production owner (reverse path) expression))
        (set! next-id (+ next-id 1))
        (set! productions (cons production productions))))
    (def (fresh owner)
      (let (name
            (string->symbol
             (string-append "$" (symbol->string owner) "."
                            (number->string next-synthetic))))
        (set! next-synthetic (+ next-synthetic 1))
        name))
    ;; Paths are allocated only for an explicitly requested diagnostic replay.
    ;; Canonical productions retain their existing shape and ordering.
    (def (child-path path index) (and observe-origin (cons index path)))
    (def (map-children procedure children path)
      (if observe-origin
        (let loop ((rest children) (index 0))
          (if (null? rest) '()
            (cons (procedure (car rest) (child-path path index))
                  (loop (cdr rest) (+ index 1)))))
        (map (lambda (child) (procedure child #f)) children)))
    (def (lower-symbol owner expression precedence path)
      (case (car expression)
        ((literal layout-start layout-next)
         (list 'terminal (car expression) (cadr expression)))
        ((token) (list 'terminal 'token (cadr expression)))
        ((reference) (list 'nonterminal (cadr expression)))
        (else
         (let (name (fresh owner))
           (lower-to name owner expression precedence path)
           (list 'nonterminal name)))))
    (def (lower-operand owner expression precedence path)
      (case (car expression)
        ((field)
         (operand-add-action
          (lower-operand owner (caddr expression) precedence (child-path path 0))
          (list 'field (cadr expression))))
        ((alias)
         (operand-add-action
          (lower-operand owner (caddr expression) precedence (child-path path 0))
          (list 'alias (cadr expression))))
        (else (lower-symbol owner expression precedence path))))
    (def (lower-to lhs owner expression precedence path)
      (def (emit lhs rhs action precedence)
        (emit-production lhs rhs action precedence owner path expression))
      (case (car expression)
        ((precedence)
         (let ((direction (cadr expression))
               (rank (caddr expression)))
           (when (and (eq? direction 'dynamic) (not allow-dynamic?))
             (error "dynamic precedence requires selective GLR admission"
                    owner rank))
           (lower-to lhs owner (cadddr expression)
                     (list direction rank) (child-path path 0))))
        ((choice)
         (if observe-origin
           (let loop ((children (cdr expression)) (index 0))
             (unless (null? children)
               (lower-to lhs owner (car children) precedence (child-path path index))
               (loop (cdr children) (+ index 1))))
           (for-each (lambda (child) (lower-to lhs owner child precedence #f))
                     (cdr expression))))
        ((empty) (emit lhs '() 'concat precedence))
        ((layout-end)
         (emit lhs '() (if (null? (cdr expression)) 'layout-end expression)
               precedence))
        ((sequence)
         (emit lhs
               (map-children (lambda (child path)
                               (lower-operand owner child precedence path))
                             (cdr expression) path)
               'concat precedence))
        ((optional)
         (emit lhs '() 'concat precedence)
         (emit lhs
               (list (lower-operand owner (cadr expression) precedence (child-path path 0)))
               'pass precedence))
        ((repeat)
         (emit lhs '() 'concat precedence)
         (emit lhs
               (list (list 'nonterminal lhs)
                     (lower-operand owner (cadr expression) precedence (child-path path 0)))
               'concat precedence))
        ((repeat1)
         (let (child (lower-operand owner (cadr expression) precedence (child-path path 0)))
           (emit lhs (list child) 'pass precedence)
           (emit lhs (list (list 'nonterminal lhs) child)
                 'concat precedence)))
        ((field)
         (emit lhs (list (lower-operand owner expression precedence path))
               'pass precedence))
        ((alias)
         (emit lhs (list (lower-operand owner expression precedence path))
               'pass precedence))
        ((literal layout-start layout-next token reference)
         (emit lhs (list (lower-symbol owner expression precedence path))
               'pass precedence))
        (else (error "unsupported GrammarExpr in LR lowering" owner expression))))
    (for-each
     (lambda (row) (lower-to (car row) (car row) (cadr row) #f '()))
     rules)
    (cons (make-production 0 '$accept
                           (list (list 'nonterminal root)) 'pass #f)
          (reverse productions))))

;;; Freezes ordered productions into the indexed representation shared by
;;; fixed-point analysis and runtime reduction; production ids remain offsets.
;; production-table
;;   : (forall (p) (-> [p] [p]))
;;   : (-> List Vector)
;;   | doc m%
;;       `production-table` publishes the canonical production index.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-table productions)
;;       ;; => vector whose index equals each production id
;;       ```
;;     %
(def (production-table productions)
  (list->vector productions))

;; production-index-by-lhs
;;   : (forall (p) (-> [p] Table))
;;   : (-> List Table)
;;   | doc m%
;;       `production-index-by-lhs` builds the shared nonterminal expansion index.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-index-by-lhs productions)
;;       ;; => table of source-ordered productions by lhs
;;       ```
;;     %
(def (production-index-by-lhs productions)
  (let (index (make-table test: eq?))
    (for-each
     (lambda (production)
       (let* ((lhs (production-lhs production))
              (found (table-ref index lhs '())))
         (table-set! index lhs (cons production found))))
    (reverse productions))
    index))

;; production-terminal-catalog
;; : (forall (p) (-> [p] [(Pair Symbol Fixnum)]))
;; production-terminal-catalog
;;   : (-> List List)
;;   | doc m%
;;       `production-terminal-catalog` binds terminal order to its index.
;;
;;       # Examples
;;
;;       ```scheme
;;       (production-terminal-catalog productions)
;;       ;; => ordered terminals and their shared index
;;       ```
;;     %
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

;; sequence-nullable?
;; : (-> List Table Boolean)
(def sequence-nullable? rhs-nullable?)

;;; Validate original repetition operands with the lowered grammar's converged
;;; nullable index. Helpers remain private; diagnostics retain the source rule
;;; and operand, including references and wrappers erased by LR lowering.
(def current-grammar-source-map (make-parameter '()))
(defstruct nullable-origin-task (owner expression path references))

(def (validate-resolved-repetitions rules nullable)
  (def source-map (current-grammar-source-map))
  (def rule-sources (let (entry (assq 'rule source-map)) (if entry (cdr entry) '())))
  (def (occurrence owner path)
    (let (source (assq owner rule-sources))
      (list (cons 'rule owner) (cons 'expressionPath (reverse path))
            (cons 'source (if source (cdr source) '())))))
  ;; A diagnostic reports one shortest nullable dependency path, not an expanded
  ;; derivation tree. Rule sharing and cycles are visited once. The fixed point
  ;; above remains the authority that the complete operand is nullable.
  (def (witness owner expression path)
    (def rows (make-table test: eq?))
    (def visited (make-table test: eq?))
    (def facts (make-table test: eq?))
    (for-each (lambda (row) (table-set! rows (car row) (cadr row))) rules)
    (def (nullable? expr)
      (let (cached (table-ref facts expr 'unknown))
        (if (not (eq? cached 'unknown)) cached
          (let (value
                 (case (car expr)
                   ((empty layout-end repeat optional) #t)
                   ((reference) (table-ref nullable (cadr expr) #f))
                   ((repeat1) (nullable? (cadr expr)))
                   ((sequence) (every nullable? (cdr expr)))
                   ((choice) (any nullable? (cdr expr)))
                   ((field alias) (nullable? (caddr expr)))
                   ((precedence) (nullable? (cadddr expr)))
                   (else #f)))
            (table-set! facts expr value) value))))
    (def front (list (make-nullable-origin-task owner expression path '())))
    (def back '())
    (def (enqueue owner expr path references)
      (set! back (cons (make-nullable-origin-task owner expr path references) back)))
    (let loop ()
      (when (null? front) (set! front (reverse back)) (set! back '()))
      (if (null? front) #f
        (let* ((task (car front))
               (owner (nullable-origin-task-owner task))
               (expr (nullable-origin-task-expression task))
               (path (nullable-origin-task-path task))
               (references (nullable-origin-task-references task)))
          (set! front (cdr front))
          (case (car expr)
            ((empty layout-end repeat optional) (reverse references))
            ((reference)
             (let ((name (cadr expr)) (row (table-ref rows (cadr expr) #f)))
               (when (and row (table-ref nullable name #f) (not (table-ref visited name #f)))
                 (table-set! visited name #t)
                 (enqueue name row '()
                          (cons (append (occurrence name '())
                                        (list (cons 'referenceOrigin (occurrence owner path)))) references))))
             (loop))
            ((sequence choice)
             (for-each
              (lambda (child index)
                (when (nullable? child) (enqueue owner child (cons index path) references)))
              (cdr expr) (iota (length (cdr expr))))
             (loop))
            ((repeat1) (enqueue owner (cadr expr) (cons 0 path) references) (loop))
            ((field alias) (enqueue owner (caddr expr) (cons 0 path) references) (loop))
            ((precedence) (enqueue owner (cadddr expr) (cons 0 path) references) (loop))
            (else (loop)))))))
  (def (reject owner expression operand path)
    (let (arguments (list owner (car expression) operand))
      (apply error "resolved repetition operand accepts empty input"
             (if (null? source-map) arguments
               (append arguments
                       (list (list (cons 'expressionOrigin (occurrence owner path))
                                   (cons 'nullableReferencePath
                                         (witness owner operand (cons 0 path))))))))))
  ;; One postorder pass validates every child and computes its nullability.
  ;; Boolean short-circuiting must never skip a nested repetition obligation.
  ;; Paths are only materialized when diagnostic context was supplied.
  (def (child-path path index)
    (if (null? source-map) '() (cons index path)))
  (def (visit owner expression path)
    (case (car expression)
      ((empty layout-end) #t)
      ((reference) (table-ref nullable (cadr expression) #f))
      ((repeat repeat1)
       (let (operand (cadr expression))
         (when (visit owner operand (child-path path 0))
           (reject owner expression operand path))
         (eq? (car expression) 'repeat)))
      ((sequence choice)
       (let loop ((children (cdr expression)) (index 0)
                  (found (eq? (car expression) 'sequence)))
         (if (null? children) found
           (let (nullable? (visit owner (car children) (child-path path index)))
             (loop (cdr children) (+ index 1)
                   (if (eq? (car expression) 'sequence)
                     (and found nullable?) (or found nullable?)))))))
      ((optional) (visit owner (cadr expression) (child-path path 0)) #t)
      ((field alias) (visit owner (caddr expression) (child-path path 0)))
      ((precedence) (visit owner (cadddr expression) (child-path path 0)))
      (else #f)))
  (for-each (lambda (row) (visit (car row) (cadr row) '())) rules))

;;; Computes nullable nonterminals to a monotone fixed point; the returned list
;;; and membership table are materialized after the dependency worklist drains.
;; compute-nullable
;; : (forall (p) (-> [p] [(Pair Symbol Datum)]))
;; compute-nullable
;;   : (-> List List)
;;   | doc m%
;;       `compute-nullable` derives the complete nullable nonterminal set.
;;
;;       # Examples
;;
;;       ```scheme
;;       (compute-nullable productions)
;;       ;; => ordered nullable names and their shared membership index
;;       ```
;;     %
;;; Two Boolean domains share one occurrence graph. Bit 1 is nullable, bit 2
;;; productive; nullable publication also admits productivity. Pending changes
;;; coalesce while queued, so a name's dependencies are visited at most twice.
(defstruct completion-clause (owner nullable-remaining productive-remaining))

(def (compute-completion productions)
  (let* ((names (nonterminals productions))
         (count (length names))
         (indices (make-table test: eq?))
         (facts (make-vector count 0))
         (pending (make-vector count 0))
         (dependents (make-vector count '()))
         (work '()))
    (for-each (lambda (name index) (table-set! indices name index)) names (iota count))
    (def (publish! index bits)
      (let* ((before (vector-ref facts index))
             (delta (bitwise-and bits (bitwise-not before))))
        (unless (zero? delta)
          (vector-set! facts index (bitwise-ior before delta))
          (when (zero? (vector-ref pending index)) (set! work (cons index work)))
          (vector-set! pending index (bitwise-ior (vector-ref pending index) delta)))))
    (for-each
     (lambda (production)
       (let ((references '()) (nullable? #t) (remaining 0)
             (owner (table-ref indices (production-lhs production))))
         (for-each
          (lambda (value)
            (let (symbol (base-symbol value))
              (if (nonterminal-symbol? symbol)
                (begin
                  ;; Missing names keep an obligation but have no publisher.
                  (set! remaining (+ remaining 1))
                  (let (index (table-ref indices (nonterminal-name symbol) #f))
                    (when index (set! references (cons index references)))))
                (set! nullable? #f))))
          (production-rhs production))
         (if (zero? remaining)
           (publish! owner (if nullable? 3 2))
           (let (clause (make-completion-clause owner (and nullable? remaining) remaining))
             ;; Register occurrences once, including duplicates, before draining.
             (for-each (lambda (index)
                         (vector-set! dependents index
                                      (cons clause (vector-ref dependents index)))) references)))))
     productions)
    (let propagate ()
      (unless (null? work)
        (let* ((index (car work)) (delta (vector-ref pending index)))
          (set! work (cdr work))
          (vector-set! pending index 0)
          (for-each
           (lambda (clause)
             (when (and (not (zero? (bitwise-and delta 1)))
                        (completion-clause-nullable-remaining clause))
               (let (remaining (- (completion-clause-nullable-remaining clause) 1))
                 (completion-clause-nullable-remaining-set! clause remaining)
                 (when (zero? remaining) (publish! (completion-clause-owner clause) 3))))
             (when (not (zero? (bitwise-and delta 2)))
               (let (remaining (- (completion-clause-productive-remaining clause) 1))
                 (completion-clause-productive-remaining-set! clause remaining)
                 (when (zero? remaining) (publish! (completion-clause-owner clause) 2)))))
           (vector-ref dependents index))
          (propagate))))
    ;; Public Boolean tables and first-lhs order are materialized only after both
    ;; domains converge; queue order and discovery order never escape the solver.
    (let ((nullable (make-table test: eq?)) (productive (make-table test: eq?))
          (nullable-names '()) (productive-names '()))
      (for-each
       (lambda (name index)
         (let (bits (vector-ref facts index))
           (unless (zero? (bitwise-and bits 1))
             (table-set! nullable name #t) (set! nullable-names (cons name nullable-names)))
           (unless (zero? (bitwise-and bits 2))
             (table-set! productive name #t) (set! productive-names (cons name productive-names)))))
       names (iota count))
      (values (reverse nullable-names) nullable (reverse productive-names) productive))))

;;; Standalone observations are projections of the same completed solution.
(def (compute-nullable productions)
  (let-values (((nullable nullable-index _productive _productive-index)
                (compute-completion productions)))
    (values nullable nullable-index)))

(def (compute-productive productions)
  (let-values (((_nullable _nullable-index productive productive-index)
                (compute-completion productions)))
    (values productive productive-index)))

;;; LR parser entries require a finite derivation. Dead alternatives/rules stay
;;; in the CFG; only an unproductive selected start is an admission error.
;;; Explain blockers in selected author expressions, never synthetic helpers.
(def (validate-resolved-start rules root productive)
  (unless (table-ref productive root #f)
    (let ((rows (make-table test: eq?))
          (seen (make-table test: eq?))
          (facts (make-table test: eq?))
          (sources (let (entry (assq 'rule (current-grammar-source-map)))
                     (if entry (cdr entry) '()))))
      (for-each (lambda (row) (table-set! rows (car row) (cadr row))) rules)
      (def (origin owner path)
        (let (source (assq owner sources))
          (list (cons 'rule owner) (cons 'expressionPath (reverse path))
                (cons 'source (if source (cdr source) '())))))
      (def (productive? expr)
        (let (cached (table-ref facts expr 'unknown))
          (if (not (eq? cached 'unknown)) cached
            (let (value
                   (case (car expr)
                     ((empty layout-end literal token layout-start layout-next repeat optional) #t)
                     ((reference) (table-ref productive (cadr expr) #f))
                     ((sequence) (every productive? (cdr expr)))
                     ((choice) (any productive? (cdr expr)))
                     ((repeat1) (productive? (cadr expr)))
                     ((field alias) (productive? (caddr expr)))
                     ((precedence) (productive? (cadddr expr)))
                     (else #f)))
              (table-set! facts expr value) value))))
      (def pending (list root))
      (table-set! seen root #t)
      (def blockers '())
      (def (visit owner expr path)
        (case (car expr)
          ((reference)
           (let (name (cadr expr))
             (unless (table-ref seen name #f)
               (table-set! seen name #t)
               (set! pending (cons name pending)))
             (list (list (cons 'rule name) (cons 'referenceOrigin (origin owner path))))))
          ((choice)
           ;; Every alternative is blocked. A single cyclic path is not a proof.
           (apply append (map (lambda (child index) (visit owner child (cons index path)))
                              (cdr expr) (iota (length (cdr expr))))))
          ((sequence)
           ;; One failed operand suffices to block this complete sequence.
           (let loop ((children (cdr expr)) (index 0))
             (cond ((null? children) '())
                   ((productive? (car children)) (loop (cdr children) (+ index 1)))
                   (else (visit owner (car children) (cons index path))))))
          ((repeat1) (visit owner (cadr expr) (cons 0 path)))
          ((field alias) (visit owner (caddr expr) (cons 0 path)))
          ((precedence) (visit owner (cadddr expr) (cons 0 path)))
          (else '())))
      (let loop ()
        (unless (null? pending)
          (let* ((owner (car pending)) (expr (table-ref rows owner #f)))
            (set! pending (cdr pending))
            (set! blockers
              (cons (list (cons 'ruleOrigin (origin owner '()))
                          (cons 'defined? (and expr #t))
                          (cons 'blockedReferences (if expr (visit owner expr '()) '()))) blockers))
            (loop))))
      (error "LR start rule has no finite terminal derivation" root
             (list (cons 'startOrigin (origin root '()))
                   (cons 'unproductiveRuleBlockers (reverse blockers)))))))

(def (symbol-first symbol first)
  (let (symbol (base-symbol symbol))
    (if (terminal-symbol? symbol)
      (list symbol)
      (table-ref first (nonterminal-name symbol) '()))))

;; sequence-first-from
;; : (-> List Table Table (Maybe Datum) List List)
(def (sequence-first-from symbols first nullable tail-lookahead found)
  (cond
   ((null? symbols)
    (if tail-lookahead (append-unique found tail-lookahead) found))
   (else
    (let* ((symbol (car symbols))
           (next (union-values found (symbol-first symbol first))))
      (if (symbol-nullable? symbol nullable)
        (sequence-first-from
         (cdr symbols) first nullable tail-lookahead next)
        next)))))

;; sequence-first
;; : (-> List Table Table (Maybe Datum) List)
;; sequence-first
;;   : (forall (a) (-> [a] Table Table a [a]))
;;   : (-> List Table Table Datum List)
;;   | doc m%
;;       `sequence-first` resolves terminals through nullable prefixes.
;;
;;       # Examples
;;
;;       ```scheme
;;       (sequence-first rhs first nullable eof)
;;       ;; => terminals reachable from the sequence prefix
;;       ```
;;     %
(def (sequence-first symbols first nullable (tail-lookahead #f))
  (sequence-first-from symbols first nullable tail-lookahead '()))

;;; Computes FIRST terminal sets after nullable convergence. Delta propagation
;;; completes before canonical terminal rows and their lookup table are published.
;; compute-first
;; : (forall (p) (-> [p] Table [(Pair Symbol Datum)]))
;; compute-first
;;   : (-> List Table List)
;;   | doc m%
;;       `compute-first` derives canonical FIRST sets for every nonterminal.
;;
;;       # Examples
;;
;;       ```scheme
;;       (compute-first productions nullable)
;;       ;; => ordered FIRST rows and their completed lookup index
;;       ```
;;     %
(def (compute-first productions nullable)
  (let-values (((terminal-values terminal-index)
                (production-terminal-catalog productions)))
    (let* ((names (nonterminals productions))
           (count (length names))
           (name-index (make-table test: eq?))
           (first-masks (make-vector count 0))
           (pending (make-vector count 0))
           (dependents (make-vector count '()))
           (edges (make-table test: eqv?))
           (work '()))
      (for-each (lambda (name index) (table-set! name-index name index)) names (iota count))
      (def (merge! index mask)
        (let* ((before (vector-ref first-masks index))
               (after (compiler-index-set-union before mask)))
          (unless (= before after)
            (let (waiting (vector-ref pending index))
              (vector-set! first-masks index after)
              (vector-set! pending index
                           (compiler-index-set-union waiting
                             (compiler-index-set-difference after before)))
              (when (zero? waiting) (set! work (cons index work)))))))
      (def (depend! name target)
        (let (source (table-ref name-index name #f))
          ;; An undefined source has no FIRST facts. Retain the existing empty
          ;; lookup semantics; reference admission remains a separate owner.
          (when source
            (let (edge (+ (* source count) target))
              (unless (table-ref edges edge #f)
                (table-set! edges edge #t)
                (vector-set! dependents source
                             (cons target (vector-ref dependents source))))))))
      ;; FIRST(lhs) depends on each nonterminal in the completed nullable
      ;; prefix, including its first nonnullable operand. A terminal seeds lhs
      ;; and blocks the remainder; marked operands retain the same base symbol.
      (for-each
       (lambda (production)
         (let ((lhs (table-ref name-index (production-lhs production))))
           (let prefix ((rhs (production-rhs production)))
             (unless (null? rhs)
               (let (symbol (base-symbol (car rhs)))
                 (if (terminal-symbol? symbol)
                   (merge! lhs (compiler-index-set-singleton
                                (table-ref terminal-index symbol)))
                   (begin
                     (depend! (nonterminal-name symbol) lhs)
                     (when (symbol-nullable? symbol nullable)
                       (prefix (cdr rhs))))))))))
       productions)
      ;; Coalesce pending facts per dense nonterminal index and propagate only
      ;; newly admitted bits. All edges exist before propagation begins.
      (let propagate ()
        (unless (null? work)
          (let* ((index (car work)) (delta (vector-ref pending index)))
            (set! work (cdr work))
            (vector-set! pending index 0)
            (for-each (lambda (target) (merge! target delta))
                      (vector-ref dependents index))
            (propagate))))
      (let (first (make-table test: eq?))
        (for-each
         (lambda (name index)
           (table-set! first name
                       (compiler-index-set->ordered-values
                        (vector-ref first-masks index) terminal-values)))
         names (iota count))
        (values (map (lambda (name) (cons name (table-ref first name))) names)
                first)))))

;;; Reads a field from the canonical LR spec association list without deriving
;;; alternate state; absent fields remain false and therefore fail closed.
;; lr-spec-ref
;;   : (forall (k v) (-> [(Pair k v)] k (Maybe v)))
;;   : (-> Alist Symbol Datum)
;;   | doc m%
;;       `lr-spec-ref` projects one canonical LR spec field by symbolic key.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lr-spec-ref spec 'actions)
;;       ;; => the immutable action-table vector, or #f when absent
;;       ```
;;     %
(def (lr-spec-ref spec key)
  (alist-ref spec key))
