;;; -*- Gerbil -*-
;;; Immutable LR table execution and lossless recognition reduction.

(import (only-in :std/vector/vector vector-map/index)
        (only-in ../compiler/lr
                 lr-spec-ref operand-actions production-action
                 production-lhs production-precedence production-rhs
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
                 recognition-sequence-append
                 recognition-sequence->list
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
                 layout-after-shift layout-after-end layout-current-action-row layout-productions?)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./token
                 token-end token-kind token-lexeme token-start))
(export lr-parse
        lr-parse/receipt
        lr-parse/prepared/receipt
        lr-prepare
        lr-parse/prepared
        lr-checkpoint?
        lr-initial-checkpoint
        lr-checkpoint-advance
        lr-checkpoint-advance-shifts
        lr-checkpoint-resume
        lr-checkpoint-frontier
        lr-checkpoint-feed
        lr-checkpoint-drive
        lr-checkpoint-lexical-mode
        lr-checkpoint-prefix-snapshot
        lr-prefix-snapshot-rebind
        lr-checkpoint-rebind-suffix
        lr-checkpoint-resume-suffix
        lr-lexical-mode?
        lr-lexical-mode-id
        lr-lexical-mode-terminals
        lr-runtime-lexical-mode-catalog lr-runtime-layout? lr-runtime-direct-step
        install-lr-runtime-direct-step!
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
               lexical-modes lexical-mode-catalog direct-step)
  transparent: #t)

;;; Install a generated reduction step once, before the runtime is shared.
(def (install-lr-runtime-direct-step! runtime step)
  (unless (and (lr-runtime? runtime)
               (procedure? step)
               (not (lr-runtime-direct-step runtime)))
    (error "invalid generated LR reduction step"))
  (lr-runtime-direct-step-set! runtime step))

;;; Interned parser-directed lexical expectation shared by LR states with the
;;; same terminal row.
(defstruct lr-lexical-mode (id terminals) transparent: #t)

;;; Immutable continuation of the deterministic LR machine.  The constructor
;;; remains private: every public checkpoint is tied to the exact prepared
;;; runtime and original token sequence that produced it.
(defstruct lr-checkpoint
  (runtime tokens input-end-offset states semantic-values rest
           deterministic-actions deterministic-shifts)
  transparent: #t)

;;; A reusable prefix keeps parser state and recognition values but does not
;;; retain a complete historical token stream across incremental edits.
(defstruct lr-prefix-snapshot
  (runtime states semantic-values deterministic-actions deterministic-shifts)
  transparent: #t)

;;; A deterministic failure frontier retains the exact immutable continuation
;;; and the terminals admitted by its LR state. Recovery can therefore test a
;;; local edit without replaying the already accepted prefix.
(defstruct lr-failure-frontier (checkpoint state expected-terminals)
  transparent: #t)

(def (lr-initial-checkpoint runtime tokens)
  (make-lr-checkpoint
   runtime tokens
   (fold (lambda (input-token offset)
           (max offset (token-end input-token)))
         0 tokens)
   '(0) '() tokens 0 0))

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
         #f)))))

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
  (let (materialized (recognition-sequence->list children))
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
  (foldl (lambda (action children)
           (apply-operand-action
            action children default-offset fragment-constructor))
         value
         actions))

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
          (eq? action 'layout-end))
      (foldl
       (lambda (operand value children)
         (recognition-sequence-append
          children
          (apply-operand-actions
           value (operand-actions operand) default-offset
           fragment-constructor)))
       '() rhs source-values))
     (else (error "unknown LR semantic action" action)))))

;;; Pop LR states and semantic values together. Accumulating the top-first
;;; semantic stack with cons produces the source order required by reductions.
;;; This also preserves the shared immutable suffix for GLR branches/checkpoints.
(def (pop-reduction states semantic-values count)
  (let loop ((remaining count) (states states) (semantic-rest semantic-values)
             (source-values '()))
    (if (zero? remaining)
      (values source-values semantic-rest states)
      (if (and (pair? states) (pair? semantic-rest))
        (loop (fx- remaining 1) (cdr states) (cdr semantic-rest)
              (cons (car semantic-rest) source-values))
        (error "LR reduction exceeds parser stack" count)))))

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
;; lr-parse/receipt
;; : (-> List List Integer (Values Datum List Alist))
;;   | doc m%
;;       Executes immutable LR tables and publishes selective-GLR evidence.
;;
;;       # Examples
;;
;;       ```scheme
;;       (let-values (((root rest receipt)
;;                     (lr-parse/receipt spec tokens)))
;;         (assq 'schema receipt))
;;       ;; => (schema . "gerbil-parser.selective-glr-receipt.v1")
;;       ```
;;     %
(def (lr-parse/prepared/receipt runtime tokens (branch-budget 256)
                                (initial-states '(0))
                                (initial-semantic-values '())
                                (initial-rest tokens)
                                (deterministic-prefix-actions 0)
                                (deterministic-prefix-shifts 0))
  (unless (and (integer? branch-budget) (positive? branch-budget))
    (error "selective GLR branch budget must be positive" branch-budget))
  (let* ((productions (lr-runtime-productions runtime))
         (table (lr-runtime-table runtime))
         (actions (lr-runtime-actions runtime))
         (action-index (lr-runtime-action-index runtime))
         (goto-index (lr-runtime-goto-index runtime))
         (case-insensitive? (lr-runtime-case-insensitive? runtime))
         (input-end-offset
          (fold (lambda (input-token offset)
                  (max offset (token-end input-token)))
                0 tokens))
         (branches-explored 0)
         ;; The preferred action is the deterministic continuation of a fork.
         ;; Only fallback actions consume the speculative branch budget; a
         ;; long preferred path must not fail merely because it visits many
         ;; conflict cells.
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
                (count (vector-ref
                        (lr-runtime-reduction-widths runtime)
                        production-id)))
           (let-values (((source-values remaining-values remaining-states)
                         (pop-reduction states semantic-values count)))
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
                (if (eq? (production-action production) 'layout-end)
                  (alet (frames (layout-after-end (and (pair? rest) (car rest))))
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
    ;; The prepared fast path hands its immutable checkpoint to selective GLR.
    ;; Starting from that checkpoint avoids replaying the deterministic prefix
    ;; from token zero whenever the first admitted fork is encountered.
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

(def (lr-parse/receipt spec tokens (branch-budget 256))
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
                        (direct-step-override 'installed))
  (unless (lr-checkpoint? checkpoint)
    (error "LR execution requires an immutable checkpoint" checkpoint))
  (let* ((runtime (lr-checkpoint-runtime checkpoint))
         (direct-step
          (if (eq? direct-step-override 'installed)
            (lr-runtime-direct-step runtime)
            direct-step-override))
         (table (lr-runtime-table runtime))
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
      (let-values
          (((root remaining _receipt)
            (call-with-parser-observed-phase
             observability 'selective-glr-execution
             (lambda ()
               (lr-parse/prepared/receipt
                runtime tokens 256 states semantic-values rest
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
               (remaining-budget action-budget))
      (if (or (and remaining-budget (zero? remaining-budget))
              (and shift-target (>= shifts shift-target)))
        (values
         'checkpoint
         (make-lr-checkpoint
          runtime tokens input-end-offset
          states semantic-values rest actions shifts))
        (let* ((state (car states))
               (rest
                (if (and next-input (null? rest))
                  (let (input-token
                        (next-input
                         (vector-ref
                          (lr-runtime-lexical-modes runtime) state)))
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
                 states semantic-values rest actions shifts)
                state
                (map car (vector-ref actions-table state))))
              (fallback states semantic-values rest actions shifts))
            (let (action (cdr action-row))
              (case (car action)
                ((shift)
                 (if (pair? rest)
                   (let ((next-states (cons (cadr action) states))
                         (next-values
                          (cons (list (make-recognition-child #f (car rest)))
                                semantic-values))
                         (next-actions (fx+ actions 1))
                         (next-shifts (fx+ shifts 1)))
                     (when after-shift
                       (after-shift (car rest) next-states next-values
                                    next-actions next-shifts))
                     (loop next-states next-values (cdr rest)
                           next-actions next-shifts
                           (and remaining-budget
                                (fx- remaining-budget 1))))
                   (fallback states semantic-values rest actions shifts)))
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
                                    (fx- remaining-budget 1)))
                         (fallback states semantic-values rest
                                   actions shifts)))
                     (let* ((production (vector-ref table production-id))
                            (count (vector-ref
                                    (lr-runtime-reduction-widths runtime)
                                    production-id)))
                       (let-values (((source-values remaining-values
                                      remaining-states)
                                     (pop-reduction
                                      states semantic-values count)))
                         (let* ((offset
                                 (if (pair? rest) (token-start (car rest))
                                     input-end-offset))
                                (value
                                 (reduce-value
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
                                   (fx- remaining-budget 1)))
                             (fallback
                              states semantic-values rest
                              actions shifts))))))))
                ((fork)
                 (if stop-at-fork?
                   (values
                    'fork
                    (make-lr-checkpoint
                     runtime tokens input-end-offset
                     states semantic-values rest actions shifts))
                   (fallback states semantic-values rest actions shifts)))
                ((accept)
                 (let (children
                       (and (pair? semantic-values)
                            (recognition-sequence->list
                             (car semantic-values))))
                   (if (and (pair? children)
                            (null? (cdr children))
                            (not (recognition-child-field (car children))))
                     (values
                      'accepted
                      (list (recognition-child-value (car children)) rest))
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
   (lr-checkpoint-deterministic-shifts checkpoint)))

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
   (lr-prefix-snapshot-deterministic-shifts snapshot)))

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
           (lr-checkpoint-deterministic-shifts checkpoint))))
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
