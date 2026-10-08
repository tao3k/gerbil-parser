;;; -*- Gerbil -*-
;;; Immutable LR table execution and lossless recognition reduction.

(import (only-in ./event-program event-program-append)
        (only-in ./event-reduce event-children-field event-children-alias)
        (only-in :std/vector/vector vector-map/index)
        (only-in ../compiler/lr
                 lr-spec-ref operand-actions production-action
                 production-lhs production-precedence production-rhs base-symbol
                 production-table)
        (only-in ./recognition
                 make-recognition-child make-recognition-fragment
                 recognition-child-field
                 recognition-child-value)
        (only-in ./reduce
                 recognition-children-alias recognition-children-field)
        (only-in ./funcs
                 association-row-index-ref
                 association-row-vector->index
                 make-value-interner
                 recognition-sequence-relocate
                 recognition-sequence-append
                 recognition-sequence->list recognition-sequence-for-action current-recognition-sequence-fusion-enabled?
                 value-interner-created-count
                 value-interner-hit-count
                 value-interner-intern
                 vector-intern-map)
        (only-in ./lr-action-index
                 index-action-row
                 lookup-action-entry
                 lookup-literal-action-entry
                 lr-action-row-eof
                 lr-action-row-tokens)
        (only-in ./layout
                 current-layout-columns current-layout-frames
                 layout-after-shift layout-after-end layout-current-action-row
                 layout-productions? layout-end-action?)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./token
                 make-token token? token-end token-kind token-lexeme token-start))
(export lr-checkpoint-state lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta
        lr-recognition-relocate lr-checkpoint-before-shift
        lr-checkpoint-fragment-compatible? lr-checkpoint-inject-fragment
        lr-runtime-fragment-reuse-safe?
        lr-recognition-fragment-offset lr-recognition-fragment-exit-mode
        current-lr-recognition-observer current-lr-transfer-yield-observer
        lr-recognition-fragment? lr-recognition-fragment-production-id
        lr-recognition-fragment-runtime lr-recognition-fragment-executor
        lr-recognition-fragment-entry-state
        lr-recognition-fragment-exit-state lr-recognition-fragment-children
        lr-recognition-fragment-lookahead lr-recognition-fragment-value
        lr-recognition-fragment-start lr-recognition-fragment-end
        lr-recognition-fragment-token-count lr-recognition-project
        current-lr-branch-budget lr-parse
        lr-parse/receipt
        lr-parse/prepared/receipt
        lr-prepare
        lr-parse/prepared
        lr-checkpoint? lr-initial-checkpoint lr-checkpoint-advance
        lr-checkpoint-advance-shifts lr-checkpoint-resume lr-checkpoint-frontier
        lr-checkpoint-feed lr-checkpoint-drive
        lr-checkpoint-drive/contextual
        lr-checkpoint-lexical-mode
        lr-checkpoint-prefix-snapshot
        lr-prefix-snapshot-rebind
        lr-checkpoint-rebind-suffix
        lr-checkpoint-resume-suffix
        lr-lexical-mode?
        lr-lexical-mode-id
        lr-lexical-mode-terminals
        lr-runtime-lexical-mode-catalog lr-runtime-layout? lr-runtime-direct-step lr-runtime-event-step
        install-lr-runtime-direct-step! install-lr-runtime-event-step!
        current-lr-event-program-enabled? lr-runtime-event-program?
        lr-runtime-for-current-semantic-backend
        lr-checkpoint-interned-lexical-mode-count
        lr-checkpoint-deterministic-actions
        lr-checkpoint-deterministic-shifts
        lr-checkpoint-remaining-token-count
        lr-failure-frontier?
        lr-failure-frontier-state
        lr-failure-frontier-expected-terminals
        lr-failure-frontier-remaining-tokens
        lr-failure-frontier-resume
        lr-rejection-condition?)

;; Prepared execution data. A generated reduction step may be installed once
;; during language-module initialization before the runtime is shared.
(defstruct lr-runtime
  (productions table reduction-widths actions action-index gotos goto-index
               case-insensitive? dynamic? layout?
               lexical-modes lexical-mode-catalog direct-step semantic-reducer event-step event-runtime)
  transparent: #t)

;;; Install a generated reduction step once, before the runtime is shared.
(def (install-lr-runtime-direct-step! runtime step)
  (unless (and (lr-runtime? runtime)
               (procedure? step)
               (not (lr-runtime-direct-step runtime)))
    (error "invalid generated LR reduction step"))
  (lr-runtime-direct-step-set! runtime step)
  (lr-runtime-event-runtime-set! runtime #f))

(def current-lr-event-program-enabled? (make-parameter #f))
(def (lr-runtime-event-program? runtime)
  (eq? (lr-runtime-semantic-reducer runtime) reduce-value/events))
(def (install-lr-runtime-event-step! runtime step)
  (unless (and (lr-runtime? runtime) (procedure? step)
               (not (lr-runtime-event-step runtime))
               (not (lr-runtime-event-program? runtime)))
    (error "invalid generated LR event step"))
  (lr-runtime-event-step-set! runtime step)
  (lr-runtime-event-runtime-set! runtime #f))

;;; The backend is selected at the prepared-runtime boundary. Checkpoints and
;;; retained fragments keep that runtime identity across edits; no semantic
;;; action reads a dynamic switch. Only deterministic pass/concat LR is admitted.
(def (lr-runtime-for-current-semantic-backend runtime)
  (cond
   ((or (not (current-lr-event-program-enabled?))
        (lr-runtime-event-program? runtime)) runtime)
   ;; Check the selected runtime before scanning grammar eligibility again.
   ((lr-runtime-event-runtime runtime) => identity)
   ((or (lr-runtime-layout? runtime) (lr-runtime-dynamic? runtime)
        (vector-any (lambda (row)
                      (any (lambda (entry) (eq? (cadr entry) 'fork)) row))
                    (lr-runtime-actions runtime))
        (vector-any (lambda (production)
                      (not (memq (production-action production) '(pass concat))))
                    (lr-runtime-table runtime))) runtime)
   (else
    (let (selected
          (make-lr-runtime
           (lr-runtime-productions runtime) (lr-runtime-table runtime)
           (lr-runtime-reduction-widths runtime) (lr-runtime-actions runtime)
           (lr-runtime-action-index runtime) (lr-runtime-gotos runtime)
           (lr-runtime-goto-index runtime) (lr-runtime-case-insensitive? runtime)
           #f #f (lr-runtime-lexical-modes runtime)
           (lr-runtime-lexical-mode-catalog runtime)
           (lr-runtime-event-step runtime) reduce-value/events #f #f))
      (lr-runtime-event-runtime-set! runtime selected)
      selected))))

;;; Interned parser-directed lexical expectation shared by LR states with the
;;; same terminal row.
(defstruct lr-lexical-mode (id terminals) transparent: #t)

;;; Immutable continuation of the deterministic LR machine.  The constructor
;;; remains private: every public checkpoint is tied to the exact prepared
;;; runtime and original token sequence that produced it.
(defstruct lr-checkpoint
  (runtime tokens input-end-offset states semantic-values rest
           deterministic-actions deterministic-shifts recognition-stack)
  transparent: #t)

;;; A reusable prefix keeps parser state and recognition values but does not
;;; retain a complete historical token stream across incremental edits.
(defstruct lr-prefix-snapshot
  (runtime states semantic-values deterministic-actions deterministic-shifts recognition-stack)
  transparent: #t)

;;; Private grammar structure, captured before pass/concat/alias projection.
;;; This is execution metadata, never a second published parse authority.
(defstruct lr-recognition-fragment
  (runtime executor production-id entry-state exit-state children lookahead value
           start end token-count offset terminal-yield)
  transparent: #t)

(defstruct lr-recognition-view (base delta) transparent: #t)
(def (lr-recognition-relocate piece delta)
  (if (lr-recognition-view? piece)
    (make-lr-recognition-view (lr-recognition-view-base piece)
                             (+ delta (lr-recognition-view-delta piece)))
    (make-lr-recognition-view piece delta)))

;;; A reduction shares its terminal yield instead of materializing a flat
;;; token list at every ancestor. Empty/unary grammar wrappers allocate no
;;; yield nodes; only concatenation and coordinate views need records.
(defstruct lr-terminal-yield-branch (left right))
(defstruct lr-terminal-yield-view (base delta))
(def (terminal-yield-append left right)
  (cond ((not left) right) ((not right) left)
        (else (make-lr-terminal-yield-branch left right))))
(def (terminal-yield-relocate yield delta)
  (cond ((or (not yield) (zero? delta)) yield)
        ((lr-terminal-yield-view? yield)
         (make-lr-terminal-yield-view (lr-terminal-yield-view-base yield)
                                     (+ delta (lr-terminal-yield-view-delta yield))))
        (else (make-lr-terminal-yield-view yield delta))))
(def (recognition-piece-terminal-yield piece)
  (cond ((lr-recognition-view? piece)
         (terminal-yield-relocate
          (recognition-piece-terminal-yield (lr-recognition-view-base piece))
          (lr-recognition-view-delta piece)))
        ((token? piece) piece)
        (else (lr-recognition-fragment-terminal-yield piece))))

;;; Optional transfer-work observation, independent of admission and publication.
;;; Receives the visited yield-node count once per attempted yield validation.
(def current-lr-transfer-yield-observer (make-parameter #f))

;;; Observer receives the accepted deterministic root, or #f when execution
;;; enters a path whose structure cannot be certified (GLR/layout/recovery).
(def current-lr-recognition-observer (make-parameter #f))

(def (recognition-piece-start piece)
  (cond ((lr-recognition-view? piece)
         (+ (lr-recognition-view-delta piece)
            (recognition-piece-start (lr-recognition-view-base piece))))
        ((token? piece) (token-start piece))
        (else (lr-recognition-fragment-start piece))))
(def (recognition-piece-end piece)
  (cond ((lr-recognition-view? piece)
         (+ (lr-recognition-view-delta piece)
            (recognition-piece-end (lr-recognition-view-base piece))))
        ((token? piece) (token-end piece))
        (else (lr-recognition-fragment-end piece))))
(def (recognition-piece-token-count piece)
  (cond ((lr-recognition-view? piece)
         (recognition-piece-token-count (lr-recognition-view-base piece)))
        ((token? piece) 1)
        (else (lr-recognition-fragment-token-count piece))))

(def (capture-reduction runtime executor production-id states stack rest offset value target)
  (if (not stack) #f
    (let* ((count (vector-ref (lr-runtime-reduction-widths runtime) production-id))
           (children (reverse (take stack count)))
           (remaining (drop stack count))
           (entry-state (list-ref states count))
           (lookahead (if (pair? rest)
                        (cons (token-kind (car rest)) (token-lexeme (car rest)))
                        #f))
           (node (make-lr-recognition-fragment
                  runtime executor production-id entry-state target children lookahead value
                  (if (pair? children) (recognition-piece-start (car children)) offset)
                  (if (pair? children) (recognition-piece-end (last children)) offset)
                  (foldl (lambda (piece count)
                           (+ count (recognition-piece-token-count piece)))
                         0 children) offset
                  (foldl (lambda (piece yield)
                           (terminal-yield-append yield (recognition-piece-terminal-yield piece)))
                         #f children))))
      (cons node remaining))))

;;; Iterative postorder projection reevaluates semantic actions from retained
;;; productions and token leaves. It deliberately does not trust cached values.
;;; Left-recursive trees therefore do not consume the Scheme call stack.
;;; Keep the independent replay on the materialized semantic path.
(def (lr-recognition-project root (source-tokens #f))
  (parameterize ((current-recognition-sequence-fusion-enabled? #f))
    (lr-recognition-project/materialized root source-tokens)))
(def (lr-recognition-project/materialized root source-tokens)
  (unless (or (lr-recognition-fragment? root) (lr-recognition-view? root))
    (error "recognition projection requires a grammar fragment" root))
  (let (tokens-by-start (and source-tokens (make-table test: eqv?)))
    (when tokens-by-start
      (for-each (lambda (token) (table-set! tokens-by-start (token-start token) token))
                source-tokens))
    (let loop ((pending (list (vector #f root 0))) (values-stack '()))
      (if (null? pending)
        (if (and (pair? values-stack) (null? (cdr values-stack)))
          (car values-stack) (error "invalid recognition projection stack"))
        (let* ((frame (car pending)) (piece (vector-ref frame 1))
               (delta (vector-ref frame 2)))
          (cond
           ((lr-recognition-view? piece)
            (loop (cons (vector #f (lr-recognition-view-base piece)
                                (+ delta (lr-recognition-view-delta piece))) (cdr pending))
                  values-stack))
           ((token? piece)
            (let (token
                  (if tokens-by-start
                    (table-ref tokens-by-start (+ delta (token-start piece)) #f)
                    (if (zero? delta) piece
                      (make-token (token-kind piece) (token-lexeme piece)
                                  (+ delta (token-start piece)) (+ delta (token-end piece))))))
              (unless (and token (eq? (token-kind token) (token-kind piece))
                           (equal? (token-lexeme token) (token-lexeme piece))
                           (= (token-end token) (+ delta (token-end piece))))
                (error "grammar position view does not bind the current token"))
              (loop (cdr pending)
                    (cons (list (make-recognition-child #f token)) values-stack))))
           ((vector-ref frame 0)
            (let* ((count (length (lr-recognition-fragment-children piece)))
                   (children (reverse (take values-stack count)))
                   (runtime (lr-recognition-fragment-runtime piece))
                   (production (vector-ref (lr-runtime-table runtime)
                                           (lr-recognition-fragment-production-id piece)))
                   (value (reduce-value production children
                                        (+ delta (lr-recognition-fragment-offset piece))
                                        make-recognition-fragment)))
              (loop (cdr pending) (cons value (drop values-stack count)))))
           (else
            (loop (append (map (lambda (child) (vector #f child delta))
                               (lr-recognition-fragment-children piece))
                          (cons (vector #t piece delta) (cdr pending)))
                  values-stack))))))))

;;; A deterministic failure frontier retains the exact immutable continuation
;;; and the terminals admitted by its LR state. Recovery can therefore test a
;;; local edit without replaying the already accepted prefix.
(defstruct lr-failure-frontier (checkpoint state expected-terminals)
  transparent: #t)

(def (lr-initial-checkpoint runtime tokens)
  (lr-initial-checkpoint/selected (lr-runtime-for-current-semantic-backend runtime) tokens))
(def (lr-initial-checkpoint/selected runtime tokens)
  (make-lr-checkpoint
   runtime tokens
   (fold (lambda (input-token offset)
           (max offset (token-end input-token)))
         0 tokens)
   '(0) '() tokens 0 0
   (and (current-lr-recognition-observer)
        (not (lr-runtime-layout? runtime)) '())))

(def (lr-checkpoint-remaining-token-count checkpoint)
  (length (lr-checkpoint-rest checkpoint)))

(def (lr-checkpoint-lexical-mode checkpoint)
  (vector-ref
   (lr-runtime-lexical-modes (lr-checkpoint-runtime checkpoint))
   (car (lr-checkpoint-states checkpoint))))

(def (lr-checkpoint-interned-lexical-mode-count checkpoint)
  (vector-length
   (lr-runtime-lexical-mode-catalog (lr-checkpoint-runtime checkpoint))))

(def (lr-failure-frontier-remaining-tokens frontier)
  (lr-checkpoint-rest (lr-failure-frontier-checkpoint frontier)))

;;; Distinguishes an expected typed parser rejection from an implementation or
;;; contract exception. Recovery probes may discard the former only.
(def (lr-rejection-condition? condition)
  (any (lambda (irritant)
         (and (list? irritant) (assq 'failureKind irritant)))
       (error-irritants condition)))

(def (lr-prepare spec)
  (let* ((productions (lr-spec-ref spec 'productions))
         (actions (lr-spec-ref spec 'actions))
         (gotos (lr-spec-ref spec 'gotos))
         (dynamic?
          (any (lambda (production)
                 (let (precedence (production-precedence production))
                   (and precedence (eq? (car precedence) 'dynamic))))
               productions))
         (layout? (layout-productions? productions)))
    (let-values (((modes mode-catalog)
                  (vector-intern-map
                   actions
                   (lambda (row) (map car row))
                   (lambda (terminals id)
                     (make-lr-lexical-mode id terminals)))))
      (let (table (production-table productions))
        (make-lr-runtime
         productions
         table
         (vector-map/index
          (lambda (_index production)
            (length (production-rhs production))) table)
         actions
         (vector-map/index
          (lambda (_index row) (index-action-row row)) actions)
         gotos
         (association-row-vector->index gotos)
         (lr-spec-ref spec 'case-insensitive?)
         dynamic?
         layout?
         modes
         mode-catalog
         #f reduce-value #f #f)))))

;; current-action-row
;; : (-> Vector Fixnum List Boolean (OrFalse Pair))
(def (current-action-row actions state tokens case-insensitive?)
  (let (row (vector-ref actions state))
    (if (null? tokens)
      (lr-action-row-eof row)
      ;; A literal is a contextual keyword/punctuation refinement of its
      ;; lexical token kind. It precedes the generic kind action.
      (let (input-token (car tokens))
        (if (current-layout-columns)
          (layout-current-action-row row input-token case-insensitive?)
          (or (lookup-literal-action-entry row (token-lexeme input-token))
              (and case-insensitive?
                   (string? (token-lexeme input-token))
                   (lookup-literal-action-entry
                    row (string-upcase (token-lexeme input-token))))
              (lookup-action-entry
               (lr-action-row-tokens row) (token-kind input-token))))))))

;; apply-operand-action
;; : (-> List List Fixnum List)
(def (apply-operand-action action children default-offset
                           fragment-constructor)
  (let (materialized (recognition-sequence-for-action children))
  (case (car action)
    ((field)
     (recognition-children-field
      (cadr action) materialized default-offset fragment-constructor))
    ((alias)
     (recognition-children-alias (cadr action) materialized default-offset))
    (else (error "unknown LR operand action" action)))))

;; apply-operand-actions
;; : (-> List List Fixnum List)
(def (apply-operand-actions value actions default-offset fragment-constructor)
  ;; Preserve foldl1's action order and terminal identity without a callback
  ;; capturing the offset and constructor for each decorated operand.
  (if (pair? actions)
    (apply-operand-actions
     (apply-operand-action (car actions) value default-offset fragment-constructor)
     (cdr actions) default-offset fragment-constructor)
    value))

;;; Keep the two-list reduction loop closed: offset and constructor travel
;;; as explicit parameters, so no callback captures them for each reduction.
;;; Like foldl2, consume the lists together in source order.
(def (reduce-operands operands values offset constructor children)
  (if (and (pair? operands) (pair? values))
    (reduce-operands
     (cdr operands) (cdr values) offset constructor
     (recognition-sequence-append
      children (apply-operand-actions
                (car values) (operand-actions (car operands)) offset constructor)))
    children))

;; reduce-value
;; : (-> List List Fixnum List)
(def (reduce-value production source-values default-offset
                   fragment-constructor)
  (let* ((rhs (production-rhs production))
         (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (pair? rhs) (null? (cdr rhs)))
      (apply-operand-actions
       (car source-values) (operand-actions (car rhs))
       default-offset fragment-constructor))
     ((or (eq? action 'concat) (eq? action 'pass)
          (layout-end-action? action))
      (reduce-operands rhs source-values default-offset fragment-constructor '()))
     (else (error "unknown LR semantic action" action)))))

(def (apply-operand-actions/events value actions offset ignored-constructor)
  (if (pair? actions)
    (apply-operand-actions/events
     (let (action (car actions))
       (case (car action)
         ((field) (event-children-field (cadr action) value offset))
         ((alias) (event-children-alias (cadr action) value offset))
         (else (error "unknown event semantic action" action))))
     (cdr actions) offset #f)
    value))
(def (reduce-operands/events operands values offset children)
  (if (and (pair? operands) (pair? values))
    (reduce-operands/events
     (cdr operands) (cdr values) offset
     (event-program-append
      children (apply-operand-actions/events
                (car values) (operand-actions (car operands)) offset #f)))
    children))
(def (reduce-value/events production source-values offset ignored-constructor)
  (let ((rhs (production-rhs production)) (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (pair? rhs) (null? (cdr rhs)))
      (apply-operand-actions/events (car source-values)
        (operand-actions (car rhs)) offset #f))
     ((memq action '(pass concat))
      (reduce-operands/events rhs source-values offset #f))
     (else (error "unsupported event LR production" action)))))

;;; Bind the popped immutable stack prefixes directly into the reduction body.
;;; Both private reducers stop at the RHS width, so unary reductions borrow the
;;; first semantic cell. Wider reductions build a source-order value list.
;;; The named loop passes the suffixes as locals instead of returning an
;;; intermediate multiple-value container to an immediately unpacking caller.
(defrule (with-pop-reduction states semantic-values count
                             (source-values remaining-values remaining-states)
                             body ...)
  (let* ((initial-states states) (initial-values semantic-values)
         (width count) (unary? (eqv? width 1)))
    (let pop ((remaining width) (remaining-states initial-states)
              (remaining-values initial-values)
              (source-values (if unary? initial-values '())))
      (if (zero? remaining)
        (begin body ...)
        (if (and (pair? remaining-states) (pair? remaining-values))
          (pop (fx- remaining 1) (cdr remaining-states) (cdr remaining-values)
               (if unary? source-values
                 (cons (car remaining-values) source-values)))
          (error "LR reduction exceeds parser stack" width))))))

;; goto-target
;; : (-> (Vector (Or (List Pair) HashTable)) Fixnum Symbol (OrFalse Fixnum))
(def (goto-target gotos state lhs)
  (let (row (association-row-index-ref gotos state lhs))
    (and row (cdr row))))

;; : (-> Datum List Integer [Nat] [Symbol] [Nat] List)
(def (make-candidate root rest score (ambiguities 0)
                     (winner-reason 'unique-completion)
                     (completion-count 1))
  (list root rest score ambiguities winner-reason completion-count))
(def candidate-root car)
(def candidate-rest cadr)
(def candidate-score caddr)
(def candidate-ambiguities cadddr)
(def (candidate-winner-reason candidate) (car (cddddr candidate)))
(def (candidate-completion-count candidate) (cadr (cddddr candidate)))

;;; Executes immutable tables and evaluates every admitted fork within a
;;; deterministic branch budget. Dynamic precedence scores complete branches;
;;; structurally identical ties merge and distinct equal-score ties fail closed.
;; : (-> List List Integer (Values Datum List Alist))
(def current-lr-branch-budget (make-parameter 256))

;;; GLR equivalence compares canonical trees, never rope association shapes.
(def (lr-parse/prepared/receipt runtime tokens (branch-budget (current-lr-branch-budget))
                                (initial-states '(0))
                                (initial-semantic-values '())
                                (initial-rest tokens)
                                (deterministic-prefix-actions 0)
                                (deterministic-prefix-shifts 0))
  (parameterize ((current-recognition-sequence-fusion-enabled? #f))
    (lr-parse/prepared/receipt/materialized runtime tokens branch-budget
      initial-states initial-semantic-values initial-rest
      deterministic-prefix-actions deterministic-prefix-shifts)))
(def (lr-parse/prepared/receipt/materialized runtime tokens (branch-budget (current-lr-branch-budget))
                                (initial-states '(0))
                                (initial-semantic-values '())
                                (initial-rest tokens)
                                (deterministic-prefix-actions 0)
                                (deterministic-prefix-shifts 0))
  (unless (and (integer? branch-budget) (positive? branch-budget))
    (error "selective GLR branch budget must be positive" branch-budget))
  (let* ((productions (lr-runtime-productions runtime))
         (table (lr-runtime-table runtime))
         (widths (lr-runtime-reduction-widths runtime))
         (actions (lr-runtime-actions runtime))
         (action-index (lr-runtime-action-index runtime))
         (goto-index (lr-runtime-goto-index runtime))
         (case-insensitive? (lr-runtime-case-insensitive? runtime))
         (input-end-offset
          (fold (lambda (input-token offset)
                  (max offset (token-end input-token)))
                0 tokens))
         (branches-explored 0)
         ;; Only fallback actions consume the speculative depth budget.
         (speculative-branches-explored 0)
         (speculative-depth 0)
         (max-speculative-depth 0)
         (branch-identities '())
         (branch-sites '())
         (merge-count 0)
         (successful-completions 0)
         (completion-identities '())
         ;; Every remaining stream is a shared tail of this request's input.
         ;; Partition by suffix identity and top LR state. Small state buckets
         ;; use assoc; wider buckets retain structural hashing.
         (configuration-tables (make-table test: eq?))
         (deterministic-memo-fuel 1024)
         (interned-configurations 0)
         (configuration-intern-hits 0)
         (completion-interner (make-value-interner))
         (fragment-interner (make-value-interner))
         (configuration-result-visiting (cons #f #t))
         (configuration-result-failed (cons #t #f))
         (configuration-memo-hits 0)
         (budget-exhausted? #f)
         (best-failure-state #f)
         (best-failure-rest #f)
         (best-failure-offset -1))
    (def (configuration-table rest)
      (let (found (table-ref configuration-tables rest #f))
        (or found
            (let (created (make-table test: eq?))
              (table-set! configuration-tables rest created)
              created))))
    (def (configuration-bucket-ref bucket key)
      (if (table? bucket)
        (table-ref bucket key #f)
        (let (entry (assoc key bucket))
          (and entry (cdr entry)))))
    (def (configuration-bucket-set! results state bucket key memo)
      (cond
       ((table? bucket)
        (table-set! bucket key memo))
       ((>= (length bucket) 8)
        (let (wide (make-table test: equal?))
          (for-each
           (lambda (entry)
             (table-set! wide (car entry) (cdr entry)))
           bucket)
          (table-set! wide key memo)
          (table-set! results state wide)))
       (else
        (table-set! results state (cons (cons key memo) bucket)))))
    (def (intern-fragment start end children)
      (value-interner-intern
       fragment-interner
       (list start end children)
       (lambda () (make-recognition-fragment start end children))))
    ;; Branch sites identify the actual LR conflict cell; schema/version data
    ;; stays in the receipt while state and terminal remain pure identities.
    (def (record-branch-site! state terminal)
      (let (found
            (find (lambda (row)
                    (and (= (car row) state)
                         (equal? (cadr row) terminal)))
                  branch-sites))
        (if found
          (set-car! (cddr found) (+ 1 (caddr found)))
          (set! branch-sites
                (cons (list state terminal 1) branch-sites)))))
    (def (failure-offset rest)
      (if (pair? rest)
        (token-start (car rest))
        input-end-offset))
    (def (record-failure! state rest)
      (let (offset (failure-offset rest))
        (when (> offset best-failure-offset)
          (set! best-failure-state state)
          (set! best-failure-rest rest)
          (set! best-failure-offset offset))))
    ;; Failed branches are common even in successful GLR parses. Materialize
    ;; the diagnostic only if every admitted branch has failed.
    (def (best-failure-evidence)
      (if best-failure-state
        (list
         (cons 'failureKind 'lr-no-action)
         (cons 'state best-failure-state)
         (cons 'byteOffset best-failure-offset)
         (cons 'tokenKind
               (if (pair? best-failure-rest)
                 (token-kind (car best-failure-rest)) 'eof))
         (cons 'tokenLexeme
               (and (pair? best-failure-rest)
                    (token-lexeme (car best-failure-rest))))
         (cons 'tokenStart
               (and (pair? best-failure-rest)
                    (token-start (car best-failure-rest))))
         (cons 'tokenEnd
               (and (pair? best-failure-rest)
                    (token-end (car best-failure-rest))))
         (cons 'expectedTerminals
               (map car (vector-ref actions best-failure-state))))
        '((failureKind . lr-no-complete-parse)
          (byteOffset . 0))))
    (def (record-completion! root rest)
      (set! successful-completions (+ successful-completions 1))
      (let* ((identity (list root rest))
             (canonical
              (value-interner-intern
               completion-interner identity (lambda () identity))))
        (unless (memq canonical completion-identities)
          (set! completion-identities
                (cons canonical completion-identities)))))
    (def (candidate-with-reason candidate reason)
      (make-candidate
       (candidate-root candidate) (candidate-rest candidate)
       (candidate-score candidate) (candidate-ambiguities candidate) reason
       (candidate-completion-count candidate)))
    (def (candidate-with-completion-count candidate count)
      (make-candidate
       (candidate-root candidate) (candidate-rest candidate)
       (candidate-score candidate) (candidate-ambiguities candidate)
       (candidate-winner-reason candidate) count))
    (def (better-candidate current candidate)
      (if (not current)
        candidate
        (let* ((completion-count
                (+ (candidate-completion-count current)
                   (candidate-completion-count candidate)))
               (winner
                (cond
                 ((> (candidate-score candidate) (candidate-score current))
                  (candidate-with-reason candidate 'dynamic-precedence))
                 ((< (candidate-score candidate) (candidate-score current))
                  (candidate-with-reason current 'dynamic-precedence))
                 ((< (length (candidate-rest candidate))
                     (length (candidate-rest current)))
                  (candidate-with-reason candidate 'maximal-consumption))
                 ((> (length (candidate-rest candidate))
                     (length (candidate-rest current)))
                  (candidate-with-reason current 'maximal-consumption))
                 ((and (equal? (candidate-root candidate)
                               (candidate-root current))
                       (equal? (candidate-rest candidate)
                               (candidate-rest current)))
                  (set! merge-count (+ merge-count 1))
                  (make-candidate
                   (candidate-root current) (candidate-rest current)
                   (candidate-score current)
                   (+ (candidate-ambiguities current)
                      (candidate-ambiguities candidate))
                   (if (or (> (candidate-ambiguities current) 0)
                           (> (candidate-ambiguities candidate) 0))
                     'ambiguous
                     'equivalent-merge)))
                 (else
                  ;; Retain a representative so an outer dynamic-precedence
                  ;; fork can still outrank this complete ambiguous set.
                  (make-candidate
                   (candidate-root current) (candidate-rest current)
                   (candidate-score current)
                   (+ 1 (candidate-ambiguities current)
                      (candidate-ambiguities candidate))
                   'ambiguous)))))
          (candidate-with-completion-count winner completion-count))))
    (def (try-action action terminal states semantic-values rest score fuel)
      (case (car action)
        ((shift)
         (and (pair? rest)
              (try-parse
               (cons (cadr action) states)
               (cons (list (make-recognition-child #f (car rest)))
                     semantic-values)
               (cdr rest) score fuel)))
        ((layout-shift)
         (and (pair? rest)
              (parameterize
                  ((current-layout-frames
                    (layout-after-shift (cadr action) (car rest))))
                (try-parse
                 (cons (caddr action) states)
                 (cons (list (make-recognition-child #f (car rest)))
                       semantic-values)
                 (cdr rest) score fuel))))
        ((reduce)
         (let* ((production-id (cadr action))
                (production (vector-ref table production-id))
                (count (vector-ref widths production-id)))
           (with-pop-reduction states semantic-values count
             (source-values remaining-values remaining-states)
             (let* ((offset (if (pair? rest) (token-start (car rest))
                            input-end-offset))
                (value
                 (reduce-value
                  production source-values offset intern-fragment))
                (precedence (production-precedence production))
                (next-score
                 (if (and precedence (eq? (car precedence) 'dynamic))
                   (+ score (cadr precedence))
                   score))
                (target
                 (and (pair? remaining-states)
                      (goto-target goto-index (car remaining-states)
                                   (production-lhs production)))))
           (and target
                (if (layout-end-action? (production-action production))
                  (alet (frames (layout-after-end (and (pair? rest) (car rest))
                                  (if (pair? (production-action production))
                                    (cdr (production-action production)) '())))
                    (parameterize ((current-layout-frames frames))
                      (try-parse (cons target remaining-states)
                                 (cons value remaining-values)
                                 rest next-score fuel)))
                  (try-parse (cons target remaining-states)
                             (cons value remaining-values)
                             rest next-score fuel)))))))
        ((accept)
         (and (or (not (lr-runtime-layout? runtime))
                  (null? (current-layout-frames)))
              (pair? semantic-values)
              (let (children
                    (recognition-sequence->list (car semantic-values)))
                (and (pair? children)
                     (null? (cdr children))
                     (not (recognition-child-field (car children)))
                     (let (root (recognition-child-value (car children)))
                       (record-completion! root rest)
                       (make-candidate root rest score))))))
        ((fork)
         (let loop ((branches (cdr action)) (best #f) (preferred? #t))
           (if (null? branches)
             (begin
               (when budget-exhausted?
                 (error "selective GLR branch budget exhausted"
                        (list
                         (cons 'failureKind 'selective-glr-budget-exhausted)
                         (cons 'branchBudget branch-budget)
                         (cons 'branchesExplored branches-explored)
                         (cons 'speculativeBranchesExplored
                               speculative-branches-explored)
                         (cons 'maxSpeculativeDepth max-speculative-depth)
                         (cons 'branchSites (reverse branch-sites)))))
               best)
             (begin
               (set! branches-explored (+ branches-explored 1))
               (when (and (zero? (modulo branches-explored 1000))
                          (equal? (getenv "GERBIL_PARSER_LR_TRACE" #f) "1"))
                 (displayln "[gerbil-parser-lr] branches=" branches-explored
                            " speculative-depth=" speculative-depth) (force-output))
               (unless preferred?
                 (set! speculative-branches-explored
                       (+ speculative-branches-explored 1))
                 (set! speculative-depth (+ speculative-depth 1))
                 (when (> speculative-depth max-speculative-depth)
                   (set! max-speculative-depth speculative-depth)))
               (record-branch-site! (car states) terminal)
               (set! branch-identities
                     (cons branches-explored branch-identities))
               (when (> speculative-depth branch-budget)
                 (set! budget-exhausted? #t))
               (let (candidate
                     (and (not budget-exhausted?)
                          (with-catch
                           (lambda (_) #f)
                           (lambda ()
                             (try-action (car branches) terminal
                                         states semantic-values rest score
                                         deterministic-memo-fuel)))))
                 (unless preferred?
                   (set! speculative-depth (- speculative-depth 1)))
                 (loop (cdr branches)
                       (if candidate
                         (better-candidate best candidate)
                         best)
                       #f))))))
        ((reject-nonassoc)
         (error "non-associative operator cannot be chained"
                (list
                 (cons 'failureKind 'non-associative-chain)
                 (cons 'precedence (cadr action))
                 (cons 'associativity (caddr action)))))
        (else (error "unknown LR action" action))))
    ;; Most configurations follow one action. Delay chart allocation until a
    ;; fork or a long linear path needs cycle detection and result reuse.
    ;; The bound sends a deterministic grammar cycle to the memoized engine.
    (def (try-parse states semantic-values rest score
                    (fuel deterministic-memo-fuel))
      (if (positive? fuel)
        (let (action-row
              (current-action-row action-index (car states) rest
                                  case-insensitive?))
          (cond
           ((not action-row)
            (record-failure! (car states) rest)
            #f)
           ((memq (cadr action-row) '(fork accept))
            (try-parse/memo states semantic-values rest score))
           (else
            (try-action (cdr action-row) (car action-row)
                        states semantic-values rest score (fx- fuel 1)))))
        (try-parse/memo states semantic-values rest score)))
    (def (try-parse/memo states semantic-values rest score)
      (let* ((results (configuration-table rest))
             (state (car states))
             (bucket (table-ref results state '()))
             (key (if (lr-runtime-layout? runtime)
                    (list states semantic-values score
                          (current-layout-frames))
                    (list states semantic-values score)))
             (memo (configuration-bucket-ref bucket key))
             (cached (and memo (car memo))))
        (if memo
          (set! configuration-intern-hits
                (fx+ configuration-intern-hits 1))
          (set! interned-configurations (fx+ interned-configurations 1)))
        (cond
         ((eq? cached configuration-result-failed) #f)
         ((eq? cached configuration-result-visiting) #f)
         (memo
          (set! configuration-memo-hits (fx+ configuration-memo-hits 1))
          (set! successful-completions
                (+ successful-completions
                   (candidate-completion-count cached)))
          cached)
         (else
          (let (memo (cons configuration-result-visiting #f))
            (configuration-bucket-set! results state bucket key memo)
            (let* ((action-row
                    (current-action-row
                     action-index state rest case-insensitive?))
                   (result
                    (if action-row
                      (try-action (cdr action-row) (car action-row)
                                  states semantic-values rest score 0)
                      (begin
                        (record-failure! state rest)
                        #f))))
              (set-car! memo (or result configuration-result-failed))
              result))))))
    ;; Resume selective GLR from the immutable deterministic checkpoint.
    (let (result (try-parse initial-states initial-semantic-values
                           initial-rest 0))
      (unless result
        (error "input does not match LR parser"
               (best-failure-evidence)))
      (when (> (candidate-ambiguities result) 0)
        (error
         "selective GLR ambiguity is unresolved"
         (list
          (cons 'failureKind 'selective-glr-ambiguity)
          (cons 'successfulCompletions successful-completions)
          (cons 'distinctCompletions (length completion-identities))
          (cons 'ambiguousBranches (candidate-ambiguities result))
          (cons 'dynamicScore (candidate-score result))
          (cons 'branchSites (reverse branch-sites)))))
      (values
       (candidate-root result)
       (candidate-rest result)
       (list
        (cons 'schema "gerbil-parser.selective-glr-receipt.v1")
        (cons 'branchBudget branch-budget)
        (cons 'deterministicPrefixActions deterministic-prefix-actions)
        (cons 'deterministicPrefixShifts deterministic-prefix-shifts)
        (cons 'branchesExplored branches-explored)
        (cons 'speculativeBranchesExplored
              speculative-branches-explored)
        (cons 'maxSpeculativeDepth max-speculative-depth)
        (cons 'branchIdentities (reverse branch-identities))
        (cons 'branchSites (reverse branch-sites))
        (cons 'mergedBranches merge-count)
        (cons 'ambiguousBranches (candidate-ambiguities result))
        (cons 'successfulCompletions successful-completions)
        (cons 'equivalentCompletions
              (- successful-completions (length completion-identities)))
        (cons 'distinctCompletions (length completion-identities))
        (cons 'internedConfigurations interned-configurations)
        (cons 'configurationInternHits configuration-intern-hits)
        (cons 'configurationMemoHits configuration-memo-hits)
        (cons 'internedCompletionIdentities
              (value-interner-created-count completion-interner))
        (cons 'completionIdentityInternHits
              (value-interner-hit-count completion-interner))
        (cons 'internedRecognitionFragments
              (value-interner-created-count fragment-interner))
        (cons 'recognitionFragmentInternHits
              (value-interner-hit-count fragment-interner))
        (cons 'winnerReason (candidate-winner-reason result))
        (cons 'dynamicScore (candidate-score result)))))))

(def (lr-parse/receipt spec tokens (branch-budget (current-lr-branch-budget)))
  (lr-parse/prepared/receipt (lr-prepare spec) tokens branch-budget))

;;; Sole deterministic executor. State remains in local variables on the hot
;;; path; an immutable checkpoint object is allocated only when an explicit
;;; action budget pauses execution. Fork/failure fallback passes the raw local
;;; frontier directly to selective GLR without replay.
;; : (-> LRCheckpoint (OrFalse Nat) PooFlowDebugCallPolicy
;;        (Values Symbol Datum))
(def (lr-run-checkpoint checkpoint action-budget observability
                        (stop-at-failure? #f) (shift-target #f)
                        (stop-at-fork? #f) (feed-token #f)
                        (next-input #f) (after-shift #f)
                        (direct-step-override 'installed)
                        (next-input-state? #f) (stop-before-shift? #f))
  (unless (lr-checkpoint? checkpoint)
    (error "LR execution requires an immutable checkpoint" checkpoint))
  (let* ((runtime (lr-checkpoint-runtime checkpoint))
         (direct-step
          (if (eq? direct-step-override 'installed)
            (lr-runtime-direct-step runtime)
            direct-step-override))
         (semantic-reducer (lr-runtime-semantic-reducer runtime))
         (event-semantics? (eq? semantic-reducer reduce-value/events))
         (table (lr-runtime-table runtime))
         (widths (lr-runtime-reduction-widths runtime))
         (modes (lr-runtime-lexical-modes runtime))
         (actions-table (lr-runtime-actions runtime))
         (action-index (lr-runtime-action-index runtime))
         (goto-index (lr-runtime-goto-index runtime))
         (case-insensitive? (lr-runtime-case-insensitive? runtime))
         (tokens
          (if feed-token
            (cons feed-token (lr-checkpoint-tokens checkpoint))
            (lr-checkpoint-tokens checkpoint)))
         (input-end-offset
          (if feed-token
            (max (lr-checkpoint-input-end-offset checkpoint)
                 (token-end feed-token))
            (lr-checkpoint-input-end-offset checkpoint))))
    (def (fallback states semantic-values rest actions shifts)
      (when (current-lr-recognition-observer)
        ((current-lr-recognition-observer) #f))
      (let-values
          (((root remaining _receipt)
            (call-with-parser-observed-phase
             observability 'selective-glr-execution
             (lambda ()
               (lr-parse/prepared/receipt
                runtime tokens (current-lr-branch-budget) states semantic-values rest
                actions shifts)))))
        (values 'accepted (list root remaining))))
    (let loop ((states (lr-checkpoint-states checkpoint))
               (semantic-values
                (lr-checkpoint-semantic-values checkpoint))
               (rest (if feed-token
                       (list feed-token)
                       (lr-checkpoint-rest checkpoint)))
               (actions (lr-checkpoint-deterministic-actions checkpoint))
               (shifts (lr-checkpoint-deterministic-shifts checkpoint))
               (remaining-budget action-budget)
               (recognition-stack (lr-checkpoint-recognition-stack checkpoint)))
      (if (or (and remaining-budget (zero? remaining-budget))
              (and shift-target (>= shifts shift-target)))
        (values
         'checkpoint
         (make-lr-checkpoint
          runtime tokens input-end-offset
          states semantic-values rest actions shifts recognition-stack))
        (let* ((state (car states))
               (rest
                (if (and next-input (null? rest))
                  (let (input-token
                        (if next-input-state?
                          (next-input
                           (vector-ref modes state)
                           state)
                          (next-input
                           (vector-ref modes state))))
                    (if input-token
                      (begin
                        (set! tokens (cons input-token tokens))
                        (set! input-end-offset
                              (max input-end-offset
                                   (token-end input-token)))
                        (list input-token))
                      '()))
                  rest))
               (action-row
                (current-action-row
                 action-index state rest case-insensitive?)))
          (if (not action-row)
            (if stop-at-failure?
              (values
               'failure
               (make-lr-failure-frontier
                (make-lr-checkpoint
                 runtime tokens input-end-offset
                 states semantic-values rest actions shifts recognition-stack)
                state
                (map car (vector-ref actions-table state))))
              (fallback states semantic-values rest actions shifts))
            (let (action (cdr action-row))
              (case (car action)
                ((shift)
                 (if stop-before-shift?
                   (values 'checkpoint
                           (make-lr-checkpoint runtime tokens input-end-offset
                             states semantic-values rest actions shifts recognition-stack))
                 (if (pair? rest)
                   (let ((next-states (cons (cadr action) states))
                         (next-values
                          (cons (if event-semantics? (car rest)
                                    (list (make-recognition-child #f (car rest))))
                                semantic-values))
                         (next-actions (fx+ actions 1))
                         (next-shifts (fx+ shifts 1)))
                     (when after-shift
                       (after-shift (car rest) next-states next-values
                                    next-actions next-shifts))
                     (loop next-states next-values (cdr rest)
                           next-actions next-shifts
                           (and remaining-budget
                                (fx- remaining-budget 1))
                           (and recognition-stack
                                (cons (car rest) recognition-stack))))
                   (fallback states semantic-values rest actions shifts))))
                ((reduce)
                 (let (production-id (cadr action))
                   (if direct-step
                     (let-values (((target next-states next-values)
                                   (direct-step
                                    production-id states semantic-values rest
                                    input-end-offset goto-index)))
                       (if target
                         (loop next-states next-values rest
                               (fx+ actions 1) shifts
                               (and remaining-budget
                                    (fx- remaining-budget 1))
                               (and recognition-stack
                                    (capture-reduction
                                     runtime direct-step production-id states recognition-stack rest
                                     (if (pair? rest) (token-start (car rest)) input-end-offset)
                                     (car next-values) target)))
                         (fallback states semantic-values rest
                                   actions shifts)))
                     (let* ((production (vector-ref table production-id))
                            (count (vector-ref widths production-id)))
                       (with-pop-reduction states semantic-values count
                         (source-values remaining-values remaining-states)
                         (let* ((offset
                                 (if (pair? rest) (token-start (car rest))
                                     input-end-offset))
                                (value
                                 (semantic-reducer
                                  production source-values offset
                                  make-recognition-fragment))
                                (target
                                 (and (pair? remaining-states)
                                      (goto-target
                                       goto-index (car remaining-states)
                                       (production-lhs production)))))
                           (if target
                             (loop
                              (cons target remaining-states)
                              (cons value remaining-values)
                              rest (fx+ actions 1) shifts
                              (and remaining-budget
                                   (fx- remaining-budget 1))
                              (and recognition-stack
                                   (capture-reduction runtime direct-step production-id states
                                     recognition-stack rest offset value target)))
                             (fallback
                              states semantic-values rest
                              actions shifts))))))))
                ((fork)
                 (if stop-at-fork?
                   (begin
                    (when (current-lr-recognition-observer)
                      ((current-lr-recognition-observer) #f))
                    (values
                    'fork
                    (make-lr-checkpoint
                     runtime tokens input-end-offset
                     states semantic-values rest actions shifts #f)))
                   (fallback states semantic-values rest actions shifts)))
                ((accept)
                 (let (children
                       (and (pair? semantic-values)
                            (recognition-sequence->list
                             (car semantic-values))))
                   (if (and (pair? children)
                            (null? (cdr children))
                            (not (recognition-child-field (car children))))
                     (begin
                       (when (current-lr-recognition-observer)
                         ((current-lr-recognition-observer)
                          (and recognition-stack (pair? recognition-stack)
                               (null? (cdr recognition-stack))
                               (car recognition-stack))))
                       (values
                        'accepted
                        (list (recognition-child-value (car children)) rest)))
                     (fallback
                      states semantic-values rest actions shifts))))
                (else
                 (fallback states semantic-values rest actions shifts))))))))))

;;; Advances through the same executor and materializes one checkpoint only at
;;; the requested deterministic action boundary.
(def (lr-checkpoint-advance checkpoint (action-budget 1)
                            (observability #f))
  (unless (and (integer? action-budget) (positive? action-budget))
    (error "LR checkpoint action budget must be positive" action-budget))
  (lr-run-checkpoint checkpoint action-budget observability))

;;; Advances by consumed significant tokens rather than raw actions. This is
;;; the stable boundary used by incremental parsing because reductions do not
;;; consume source input.
(def (lr-checkpoint-advance-shifts checkpoint (shift-budget 1)
                                   (observability #f))
  (unless (and (integer? shift-budget) (positive? shift-budget))
    (error "LR checkpoint shift budget must be positive" shift-budget))
  (lr-run-checkpoint
   checkpoint #f observability #f
   (+ (lr-checkpoint-deterministic-shifts checkpoint) shift-budget)))

;;; Feeds exactly one significant token through reductions and one shift. If
;;; reductions expose a selective-GLR cell first, returns its exact immutable
;;; frontier instead of starting GLR with a truncated one-token suffix.
(def (lr-checkpoint-feed checkpoint input-token (observability #f))
  (lr-run-checkpoint
   checkpoint #f observability #f
   (fx+ (lr-checkpoint-deterministic-shifts checkpoint) 1)
   #t input-token))

(def (lr-checkpoint-state checkpoint) (car (lr-checkpoint-states checkpoint)))

(def (lr-checkpoint-before-shift checkpoint token)
  (let-values (((status next)
                (lr-run-checkpoint checkpoint #f #f #t #f #t token
                                   #f #f 'installed #f #t)))
    (and (eq? status 'checkpoint) next)))

(def (lr-runtime-fragment-reuse-safe? runtime)
  (and (not (lr-runtime-layout? runtime)) (not (lr-runtime-dynamic? runtime))
       ;; Admit the repeated-structure family first. Operator ancestry has no
       ;; such concat recurrence and does not justify building a reuse catalog.
       (vector-any
        (lambda (production)
          (let (rhs (production-rhs production))
            (and (eq? (production-action production) 'concat)
                 (= (length rhs) 2)
                 (equal? (base-symbol (car rhs))
                         (list 'nonterminal (production-lhs production))))))
        (lr-runtime-table runtime))
       (not (vector-any
             (lambda (row) (any (lambda (entry) (eq? (cadr entry) 'fork)) row))
             (lr-runtime-actions runtime)))))

(def (lr-checkpoint-fragment-compatible? checkpoint fragment)
  (let ((runtime (lr-checkpoint-runtime checkpoint))
        (state (car (lr-checkpoint-states checkpoint))))
    (and (lr-recognition-fragment? fragment)
         (eq? runtime (lr-recognition-fragment-runtime fragment))
         (eq? (lr-runtime-direct-step runtime) (lr-recognition-fragment-executor fragment))
         (not (lr-runtime-layout? runtime)) (not (lr-runtime-dynamic? runtime))
         (= state (lr-recognition-fragment-entry-state fragment))
         (let (target
               (goto-target (lr-runtime-goto-index runtime) state
                            (production-lhs (vector-ref (lr-runtime-table runtime)
                              (lr-recognition-fragment-production-id fragment)))))
           (and target (= target (lr-recognition-fragment-exit-state fragment)))))))

(def (lr-recognition-fragment-exit-mode fragment)
  (vector-ref (lr-runtime-lexical-modes (lr-recognition-fragment-runtime fragment))
              (lr-recognition-fragment-exit-state fragment)))

;;; Compare retained terminal leaves without projecting the published AST.
;;; Views accumulate byte relocation; pending siblings keep their own delta.
(def (same-transfer-token? actual retained delta)
  (and (token? actual) (token? retained)
       (eq? (token-kind actual) (token-kind retained))
       (equal? (token-lexeme actual) (token-lexeme retained))
       (= (token-start actual) (+ delta (token-start retained)))
       (= (token-end actual) (+ delta (token-end retained)))))

(def (fragment-yield-matches? fragment delta tokens)
  (let ((yield (lr-recognition-fragment-terminal-yield fragment))
        (observer (current-lr-transfer-yield-observer)))
    (def (finish matched? visited)
      (when observer (observer visited))
      matched?)
    (let loop ((pending (if yield (list (cons yield delta)) '())) (rest tokens) (visited 0))
      (if (null? pending) (finish (null? rest) visited)
        (let* ((frame (car pending)) (piece (car frame)) (shift (cdr frame))
               (visited (if observer (+ visited 1) visited)))
          (cond
           ((lr-terminal-yield-view? piece)
            (loop (cons (cons (lr-terminal-yield-view-base piece)
                              (+ shift (lr-terminal-yield-view-delta piece)))
                        (cdr pending)) rest visited))
           ((lr-terminal-yield-branch? piece)
            (loop (cons (cons (lr-terminal-yield-branch-left piece) shift)
                        (cons (cons (lr-terminal-yield-branch-right piece) shift) (cdr pending)))
                  rest visited))
           ((and (pair? rest) (same-transfer-token? (car rest) piece shift))
            (loop (cdr pending) (cdr rest) visited))
           (else (finish #f visited))))))))

;;; The source owner certifies unchanged text and both lexical boundaries.
;;; The LR owner independently verifies the transferred terminal yield.
;;; Transfer one actual nonterminal, not an exported AST kind or an LR suffix.
(def (lr-checkpoint-inject-fragment checkpoint fragment delta significant-tokens)
  (unless (and (lr-checkpoint-fragment-compatible? checkpoint fragment)
               (pair? (lr-checkpoint-rest checkpoint))
               (pair? significant-tokens)
               (same-transfer-token? (car significant-tokens)
                                     (car (lr-checkpoint-rest checkpoint)) 0)
               (positive? (lr-recognition-fragment-token-count fragment))
               (= (length significant-tokens) (lr-recognition-fragment-token-count fragment))
               (fragment-yield-matches? fragment delta significant-tokens))
    (error "uncertified LR fragment transfer"))
  (let* ((piece (lr-recognition-relocate fragment delta))
         (end (recognition-piece-end piece))
         (count (lr-recognition-fragment-token-count fragment)))
    (make-lr-checkpoint
     (lr-checkpoint-runtime checkpoint)
     (append (reverse significant-tokens) (cdr (lr-checkpoint-tokens checkpoint)))
     (max (lr-checkpoint-input-end-offset checkpoint) end)
     (cons (lr-recognition-fragment-exit-state fragment) (lr-checkpoint-states checkpoint))
     (cons (recognition-sequence-relocate (lr-recognition-fragment-value fragment) delta)
           (lr-checkpoint-semantic-values checkpoint))
     '() (+ 1 (lr-checkpoint-deterministic-actions checkpoint))
     (+ count (lr-checkpoint-deterministic-shifts checkpoint))
     (and (lr-checkpoint-recognition-stack checkpoint)
          (cons piece (lr-checkpoint-recognition-stack checkpoint))))))

;;; Drives one deterministic LR loop while the source owner supplies tokens
;;; under the current lexical mode. The callback records shifted source tokens.
;;; A fork returns its exact checkpoint for the existing selective-GLR handoff.
;;; The optional override gives conformance tests and matched benchmarks an
;;; indexed baseline without mutating the installed language machine.
(def (lr-checkpoint-drive checkpoint next-input after-shift
                          (observability #f)
                          (direct-step-override 'installed))
  (lr-run-checkpoint
   checkpoint #f observability #f #f #t #f
   next-input after-shift direct-step-override))

;;; Contextual scanning needs the exact LR state as its syntactic position
;;; axis. Existing lexer drivers keep the one-argument callback contract.
(def (lr-checkpoint-drive/contextual checkpoint next-input after-shift
                                     (observability #f))
  (lr-run-checkpoint
   checkpoint #f observability #f #f #t #f
   next-input after-shift 'installed #t))

;;; Rebinds an unconsumed suffix to the same immutable deterministic frontier.
;;; Streaming GLR handoff and explicit incremental sessions share this path.
(def (lr-checkpoint-prefix-snapshot checkpoint)
  (unless (lr-checkpoint? checkpoint)
    (error "LR prefix snapshot requires a checkpoint" checkpoint))
  (make-lr-prefix-snapshot
   (lr-checkpoint-runtime checkpoint)
   (lr-checkpoint-states checkpoint)
   (lr-checkpoint-semantic-values checkpoint)
   (lr-checkpoint-deterministic-actions checkpoint)
   (lr-checkpoint-deterministic-shifts checkpoint)
   (lr-checkpoint-recognition-stack checkpoint)))

(def (lr-prefix-snapshot-rebind snapshot tokens rest
                                (input-end-offset #f))
  (unless (lr-prefix-snapshot? snapshot)
    (error "LR prefix rebind requires a snapshot" snapshot))
  (make-lr-checkpoint
   (lr-prefix-snapshot-runtime snapshot)
   tokens
   (or input-end-offset
       (fold (lambda (input-token offset)
               (max offset (token-end input-token)))
             0 tokens))
   (lr-prefix-snapshot-states snapshot)
   (lr-prefix-snapshot-semantic-values snapshot)
   rest
   (lr-prefix-snapshot-deterministic-actions snapshot)
   (lr-prefix-snapshot-deterministic-shifts snapshot)
   (lr-prefix-snapshot-recognition-stack snapshot)))

(def (lr-checkpoint-rebind-suffix checkpoint tokens rest)
  (lr-prefix-snapshot-rebind
   (lr-checkpoint-prefix-snapshot checkpoint) tokens rest))

(def (lr-checkpoint-resume-suffix checkpoint tokens rest (observability #f))
  (lr-checkpoint-resume
   (lr-checkpoint-rebind-suffix checkpoint tokens rest) observability))

;;; Completes parsing from an initial or advanced immutable checkpoint.
(def (lr-checkpoint-resume checkpoint (observability #f))
  (let-values (((status payload)
                (lr-run-checkpoint checkpoint #f observability)))
    (unless (eq? status 'accepted)
      (error "LR resume did not reach a terminal result" status))
    (values (car payload) (cadr payload))))

;;; Runs to acceptance or to the first deterministic LR failure frontier. An
;;; admitted GLR fork still delegates to the selective-GLR owner.
(def (lr-checkpoint-frontier checkpoint (observability #f))
  (lr-run-checkpoint checkpoint #f observability #t))

;;; Tests one edited suffix from the retained failure state. The returned CST
;;; remains private to recovery; only acceptance is exposed.
(def (lr-failure-frontier-resume frontier edited-rest (observability #f))
  (unless (lr-failure-frontier? frontier)
    (error "LR recovery requires a failure frontier" frontier))
  (let* ((checkpoint (lr-failure-frontier-checkpoint frontier))
         (edited
          (make-lr-checkpoint
           (lr-checkpoint-runtime checkpoint)
           (lr-checkpoint-tokens checkpoint)
           (lr-checkpoint-input-end-offset checkpoint)
           (lr-checkpoint-states checkpoint)
           (lr-checkpoint-semantic-values checkpoint)
           edited-rest
           (lr-checkpoint-deterministic-actions checkpoint)
           (lr-checkpoint-deterministic-shifts checkpoint) #f)))
    (with-catch
     (lambda (condition)
       (if (lr-rejection-condition? condition)
         #f
         (raise condition)))
     (lambda ()
       (let-values (((_root rest) (lr-checkpoint-resume edited observability)))
         (null? rest))))))

(def (lr-parse/prepared runtime tokens (observability #f))
  (if (lr-runtime-layout? runtime)
    (let-values (((root rest _receipt)
                  (lr-parse/prepared/receipt runtime tokens)))
      (values root rest))
    (lr-checkpoint-resume
     (lr-initial-checkpoint runtime tokens) observability)))
(def (lr-parse spec tokens)
  (lr-parse/prepared (lr-prepare spec) tokens))
