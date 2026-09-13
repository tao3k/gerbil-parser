;;; -*- Gerbil -*-
;;; Deterministic LALR(1) compilation and lossless recognition runtime.

(import (only-in :std/sort sort)
        (only-in ./funcs
                 compiler-index-set->ordered-values
                 compiler-index-set-singleton
                 compiler-index-set-union))
(export +lr-eof+
        compute-first
        compute-nullable
        lr-spec-ref
        lower-rules
        base-symbol
        nonterminal-name
        nonterminal-symbol?
        operand-actions
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

(def (operand-add-action value action)
  (if (marked-symbol? value)
    (list 'marked (cadr value) (append (caddr value) (list action)))
    (list 'marked value (list action))))

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
(def (lower-rules rules root (allow-dynamic? #f))
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
           (when (and (eq? direction 'dynamic) (not allow-dynamic?))
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

;;; Computes nullable nonterminals to a monotone fixed point; the returned list
;;; and membership table are materialized from the same completed iteration.
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

;;; Computes FIRST terminal sets after nullable convergence; no partially
;;; updated set escapes an iteration, preserving deterministic table identity.
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
    (let ((names (nonterminals productions))
          (first-masks (make-table test: eq?)))
      (for-each (lambda (name) (table-set! first-masks name 0)) names)
      (def (symbol-first-mask symbol)
        (let (symbol (base-symbol symbol))
          (if (terminal-symbol? symbol)
            (compiler-index-set-singleton
             (table-ref terminal-index symbol))
            (table-ref first-masks (nonterminal-name symbol) 0))))
      (def (sequence-first-mask symbols)
        (let loop ((rest symbols) (mask 0))
          (if (null? rest)
            mask
            (let* ((symbol (car rest))
                   (next
                    (compiler-index-set-union
                     mask (symbol-first-mask symbol))))
              (if (symbol-nullable? symbol nullable)
                (loop (cdr rest) next)
                next)))))
      (let fixed-point ()
        (let (changed? #f)
          (for-each
           (lambda (production)
             (let* ((lhs (production-lhs production))
                    (before (table-ref first-masks lhs 0))
                    (after
                     (compiler-index-set-union
                      before
                      (sequence-first-mask (production-rhs production)))))
               (unless (= before after)
                 (table-set! first-masks lhs after)
                 (set! changed? #t))))
           productions)
          (if changed?
            (fixed-point)
            (let (first (make-table test: eq?))
              (for-each
               (lambda (name)
                 (table-set! first name
                             (compiler-index-set->ordered-values
                              (table-ref first-masks name 0)
                              terminal-values)))
               names)
              (values
               (map (lambda (name) (cons name (table-ref first name '()))) names)
               first))))))))

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
