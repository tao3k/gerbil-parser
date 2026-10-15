;;; -*- Gerbil -*-
;;; lr-parser: state owner. Private execution state and named algorithms.

(export lr-capture-context make-lr-capture-context
        lr-capture-context-runtime lr-capture-context-executor
        lr-capture-context-production-id lr-capture-context-entry-state lr-capture-context-exit-state lr-capture-context-lookahead-kind
        lr-runtime
        make-lr-runtime
        lr-runtime?
        lr-runtime-productions
        lr-runtime-productions-set!
        lr-runtime-table
        lr-runtime-table-set!
        lr-runtime-reduction-widths
        lr-runtime-reduction-widths-set!
        lr-runtime-actions
        lr-runtime-actions-set!
        lr-runtime-action-index
        lr-runtime-action-index-set!
        lr-runtime-gotos
        lr-runtime-gotos-set!
        lr-runtime-goto-index
        lr-runtime-goto-columns
        lr-runtime-capture-contexts
        lr-runtime-capture-contexts-set!
        lr-runtime-goto-index-set!
        lr-runtime-case-insensitive?
        lr-runtime-case-insensitive?-set!
        lr-runtime-dynamic?
        lr-runtime-dynamic?-set!
        lr-runtime-layout?
        lr-runtime-layout?-set!
        lr-runtime-lexical-modes
        lr-runtime-lexical-modes-set!
        lr-runtime-lexical-mode-catalog
        lr-runtime-lexical-mode-catalog-set!
        lr-runtime-direct-step
        lr-runtime-direct-step-set!
        lr-runtime-semantic-reducer
        lr-runtime-semantic-reducer-set!
        lr-runtime-event-reducer
        lr-runtime-event-reducer-set!
        lr-runtime-event-step
        lr-runtime-event-step-set!
        lr-runtime-event-runtime
        lr-runtime-event-runtime-set!
        lr-runtime-reduction-plans
        lr-runtime-reduction-plans-set!
        lr-runtime-action-selector
        lr-runtime-action-selector-set!
        lr-runtime-owned-stack?
        lr-runtime-owned-stack?-set!
        current-lr-event-program-enabled?
        lr-lexical-mode
        make-lr-lexical-mode
        lr-lexical-mode?
        lr-lexical-mode-id
        lr-lexical-mode-id-set!
        lr-lexical-mode-terminals
        lr-lexical-mode-terminals-set!
        lr-checkpoint
        make-lr-checkpoint
        lr-checkpoint?
        lr-checkpoint-runtime
        lr-checkpoint-runtime-set!
        lr-checkpoint-tokens
        lr-checkpoint-tokens-set!
        lr-checkpoint-input-end-offset
        lr-checkpoint-input-end-offset-set!
        lr-checkpoint-states
        lr-checkpoint-states-set!
        lr-checkpoint-semantic-values
        lr-checkpoint-semantic-values-set!
        lr-checkpoint-rest
        lr-checkpoint-rest-set!
        lr-checkpoint-deterministic-actions
        lr-checkpoint-deterministic-actions-set!
        lr-checkpoint-deterministic-shifts
        lr-checkpoint-deterministic-shifts-set!
        lr-checkpoint-recognition-stack
        lr-checkpoint-recognition-stack-set!
        lr-prefix-snapshot
        make-lr-prefix-snapshot
        lr-prefix-snapshot?
        lr-prefix-snapshot-runtime
        lr-prefix-snapshot-runtime-set!
        lr-prefix-snapshot-states
        lr-prefix-snapshot-states-set!
        lr-prefix-snapshot-semantic-values
        lr-prefix-snapshot-semantic-values-set!
        lr-prefix-snapshot-deterministic-actions
        lr-prefix-snapshot-deterministic-actions-set!
        lr-prefix-snapshot-deterministic-shifts
        lr-prefix-snapshot-deterministic-shifts-set!
        lr-prefix-snapshot-recognition-stack
        lr-prefix-snapshot-recognition-stack-set!
        lr-recognition-fragment
        make-lr-recognition-fragment
        lr-recognition-fragment?
        lr-recognition-fragment-runtime
        lr-recognition-fragment-executor
        lr-recognition-fragment-production-id
        lr-recognition-fragment-entry-state
        lr-recognition-fragment-exit-state
        lr-recognition-fragment-children
        lr-recognition-fragment-children-cache
        lr-recognition-fragment-lookahead
        lr-recognition-fragment-lookahead-cache
        lr-recognition-fragment-value
        lr-recognition-fragment-value-set!
        lr-recognition-fragment-start
        lr-recognition-fragment-start-set!
        lr-recognition-fragment-end
        lr-recognition-fragment-end-set!
        lr-recognition-fragment-token-count
        lr-recognition-fragment-token-count-set!
        lr-recognition-fragment-offset
        lr-recognition-fragment-offset-set!
        lr-recognition-fragment-yield-cache
        lr-recognition-fragment-yield-cache-set!
        lr-recognition-view
        make-lr-recognition-view
        lr-recognition-view?
        lr-recognition-view-base
        lr-recognition-view-base-set!
        lr-recognition-view-delta
        lr-recognition-view-delta-set!
        lr-terminal-yield-branch
        make-lr-terminal-yield-branch
        lr-terminal-yield-branch?
        lr-terminal-yield-branch-left
        lr-terminal-yield-branch-left-set!
        lr-terminal-yield-branch-right
        lr-terminal-yield-branch-right-set!
        lr-terminal-yield-view
        make-lr-terminal-yield-view
        lr-terminal-yield-view?
        lr-terminal-yield-view-base
        lr-terminal-yield-view-base-set!
        lr-terminal-yield-view-delta
        lr-terminal-yield-view-delta-set!
        current-lr-transfer-yield-observer
        current-lr-recognition-observer
        lr-failure-frontier
        make-lr-failure-frontier
        lr-failure-frontier?
        lr-failure-frontier-checkpoint
        lr-failure-frontier-checkpoint-set!
        lr-failure-frontier-state
        lr-failure-frontier-state-set!
        lr-failure-frontier-expected-terminals
        lr-failure-frontier-expected-terminals-set!
        current-lr-branch-budget)

;;; -*- Gerbil -*-
;;; Immutable LR table execution and lossless recognition reduction.
;; Prepared execution data. A generated reduction step may be installed once
;; during language-module initialization before the runtime is shared.
(defstruct lr-runtime
  (productions table reduction-widths actions action-index gotos goto-index
               case-insensitive? dynamic? layout?
               lexical-modes lexical-mode-catalog direct-step semantic-reducer event-reducer event-step event-runtime
               reduction-plans action-selector owned-stack? goto-columns capture-contexts)
  transparent: #t final: #t)

(def current-lr-event-program-enabled? (make-parameter #f))

;;; Interned parser-directed lexical expectation shared by LR states with the
;;; same terminal row.
(defstruct lr-lexical-mode (id terminals) transparent: #t final: #t)

;;; Immutable continuation of the deterministic LR machine.  The constructor
;;; remains private: every public checkpoint is tied to the exact prepared
;;; runtime and original token sequence that produced it.
(defstruct lr-checkpoint
  (runtime tokens input-end-offset states semantic-values rest
           deterministic-actions deterministic-shifts recognition-stack)
  transparent: #t final: #t)

;;; A reusable prefix keeps parser state and recognition values but does not
;;; retain a complete historical token stream across incremental edits.
(defstruct lr-prefix-snapshot
  (runtime states semantic-values deterministic-actions deterministic-shifts recognition-stack)
  transparent: #t final: #t)

;;; Private grammar structure, captured before pass/concat/alias projection.
;;; This is execution metadata, never a second published parse authority.
;;; Immutable prepared transition identity is shared; request-local payloads
;;; remain separate in each fragment. No context mutator is exported.
(defstruct lr-capture-context (runtime executor production-id entry-state exit-state lookahead-kind)
  transparent: #t final: #t)
(defstruct lr-recognition-fragment
  (context children-cache lookahead-cache value start end token-count offset yield-cache)
  transparent: #t final: #t)
(definline (lr-recognition-fragment-lookahead (piece : lr-recognition-fragment)) => :t
  (let (stored piece.lookahead-cache)
    (if (or (not stored) (pair? stored)) stored
      (let (signature (cons (lr-capture-context-lookahead-kind piece.context) stored))
        (lr-recognition-fragment-lookahead-cache-set! piece signature)
        signature))))
(definline (lr-recognition-fragment-children (piece : lr-recognition-fragment)) => :t
  (let (stored piece.children-cache)
    (if (or (pair? stored) (null? stored)) stored
      (let (children (list stored))
        (lr-recognition-fragment-children-cache-set! piece children)
        children))))
(definline (lr-recognition-fragment-runtime (piece : lr-recognition-fragment)) => :t
  (lr-capture-context-runtime piece.context))
(definline (lr-recognition-fragment-executor (piece : lr-recognition-fragment)) => :t
  (lr-capture-context-executor piece.context))
(definline (lr-recognition-fragment-production-id (piece : lr-recognition-fragment)) => :t
  (lr-capture-context-production-id piece.context))
(definline (lr-recognition-fragment-entry-state (piece : lr-recognition-fragment)) => :t
  (lr-capture-context-entry-state piece.context))
(definline (lr-recognition-fragment-exit-state (piece : lr-recognition-fragment)) => :t
  (lr-capture-context-exit-state piece.context))

(defstruct lr-recognition-view (base delta) transparent: #t final: #t)

;;; A reduction shares its terminal yield instead of materializing a flat
;;; token list at every ancestor. Empty/unary grammar wrappers allocate no
;;; yield nodes; only concatenation and coordinate views need records.
(defstruct lr-terminal-yield-branch (left right) final: #t)

(defstruct lr-terminal-yield-view (base delta) final: #t)

;;; Optional transfer-work observation, independent of admission and publication.
;;; Receives the visited yield-node count once per attempted yield validation.
(def current-lr-transfer-yield-observer (make-parameter #f))

;;; Observer receives the accepted deterministic root, or #f when execution
;;; enters a path whose structure cannot be certified (GLR/layout/recovery).
(def current-lr-recognition-observer (make-parameter #f))

;;; A deterministic failure frontier retains the exact immutable continuation
;;; and the terminals admitted by its LR state. Recovery can therefore test a
;;; local edit without replaying the already accepted prefix.
(defstruct lr-failure-frontier (checkpoint state expected-terminals)
  transparent: #t final: #t)

;;; Executes immutable tables and evaluates every admitted fork within a
;;; deterministic branch budget. Dynamic precedence scores complete branches;
;;; structurally identical ties merge and distinct equal-score ties fail closed.
;; : (-> List List Integer (Values Datum List Alist))
(def current-lr-branch-budget (make-parameter 256))

