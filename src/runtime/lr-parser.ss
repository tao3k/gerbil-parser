;;; -*- Gerbil -*-
;;; Immutable LR table execution and lossless recognition reduction.

(import (only-in :std/srfi/1 split-at)
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
                 recognition-sequence-concatenate
                 recognition-sequence->list
                 value-interner-created-count
                 value-interner-hit-count
                 value-interner-intern)
        (only-in ./observability
                 call-with-parser-observed-phase)
        (only-in ./token
                 token-end token-kind token-lexeme token-start))
(export lr-parse
        lr-parse/receipt
        lr-prepare
        lr-parse/prepared)

(def +lr-eof+ '(terminal eof))

;; Immutable execution data derived once for generated parser machines.
(defstruct lr-runtime
  (productions table actions action-index gotos goto-index
               case-insensitive? dynamic?)
  transparent: #t)

(def (lr-prepare spec)
  (let* ((productions (lr-spec-ref spec 'productions))
         (actions (lr-spec-ref spec 'actions))
         (gotos (lr-spec-ref spec 'gotos))
         (dynamic?
          (let loop ((rest productions))
            (and (pair? rest)
                 (let (precedence (production-precedence (car rest)))
                   (or (and precedence (eq? (car precedence) 'dynamic))
                       (loop (cdr rest))))))))
    (make-lr-runtime
     productions
     (production-table productions)
     actions
     (association-row-vector->index actions)
     gotos
     (association-row-vector->index gotos)
     (lr-spec-ref spec 'case-insensitive?)
     dynamic?)))

;; lookup-action-row
;; : (-> (Vector (Or (List Pair) HashTable)) Fixnum Datum (OrFalse Pair))
(def lookup-action-row association-row-index-ref)

;; current-action-row
;; : (-> Vector Fixnum List Boolean (OrFalse Pair))
(def (current-action-row actions state tokens case-insensitive?)
  (if (null? tokens)
    (lookup-action-row actions state +lr-eof+)
    ;; A literal is a contextual keyword/punctuation refinement of its lexical
    ;; token kind. It has deterministic priority over the generic kind action.
    (or (lookup-action-row actions state
                           (list 'terminal 'literal
                                 (token-lexeme (car tokens))))
        (and case-insensitive?
             (string? (token-lexeme (car tokens)))
             (lookup-action-row actions state
                                (list 'terminal 'literal
                                      (string-upcase
                                       (token-lexeme (car tokens))))))
        (lookup-action-row actions state
                           (list 'terminal 'token
                                 (token-kind (car tokens)))))))

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
(def (reduce-value production reversed-values default-offset
                   fragment-constructor)
  (let* ((rhs (production-rhs production))
         (source-values (reverse reversed-values))
         (reduced-values
          (map (lambda (operand value)
                 (apply-operand-actions
                  value (operand-actions operand) default-offset
                  fragment-constructor))
               rhs source-values))
         (children (recognition-sequence-concatenate reduced-values))
         (action (production-action production)))
    (cond
     ((eq? action 'concat) children)
     ((eq? action 'pass)
      (if (= (length reduced-values) 1)
        (car reduced-values)
        children))
     (else (error "unknown LR semantic action" action)))))

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

;;; Canonical request-local GLR configuration. Parser stacks, semantic values,
;;; and token suffixes are immutable, so structural interning cannot leak
;;; mutation between branches or requests.
(defstruct glr-configuration (states semantic-values rest score)
  transparent: #t)

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
          (let loop ((rest tokens) (offset 0))
            (if (null? rest) offset
                (loop (cdr rest) (token-end (car rest))))))
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
         (configuration-interner (make-value-interner))
         (completion-interner (make-value-interner))
         (fragment-interner (make-value-interner))
         (configuration-results (make-table test: eq?))
         (configuration-result-missing (cons #f #f))
         (configuration-result-visiting (cons #f #t))
         (configuration-result-failed (cons #t #f))
         (configuration-memo-hits 0)
         (budget-exhausted? #f)
         (best-failure #f))
    (def (intern-configuration states semantic-values rest score)
      (value-interner-intern
       configuration-interner
       (list states semantic-values rest score)
       (lambda ()
         (make-glr-configuration states semantic-values rest score))))
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
        (when (or (not best-failure)
                  (> offset (cdr (assq 'byteOffset best-failure))))
          (set! best-failure
                (list
                 (cons 'failureKind 'lr-no-action)
                 (cons 'state state)
                 (cons 'byteOffset offset)
                 (cons 'tokenKind
                       (if (pair? rest) (token-kind (car rest)) 'eof))
                 (cons 'tokenLexeme
                       (and (pair? rest) (token-lexeme (car rest))))
                 (cons 'tokenStart
                       (and (pair? rest) (token-start (car rest))))
                 (cons 'tokenEnd
                       (and (pair? rest) (token-end (car rest))))
                 (cons 'expectedTerminals
                       (map car (vector-ref actions state))))))))
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
    (def (try-action action terminal states semantic-values rest score)
      (case (car action)
        ((shift)
         (and (pair? rest)
              (try-parse
               (cons (cadr action) states)
               (cons (list (make-recognition-child #f (car rest)))
                     semantic-values)
               (cdr rest) score)))
        ((reduce)
         (let* ((production (vector-ref table (cadr action)))
                (count (length (production-rhs production))))
           (let-values (((popped-values remaining-values)
                         (split-at semantic-values count))
                        ((_popped-states remaining-states)
                         (split-at states count)))
             (let* (
                (offset (if (pair? rest) (token-start (car rest))
                            input-end-offset))
                (value
                 (reduce-value
                  production popped-values offset intern-fragment))
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
                (try-parse (cons target remaining-states)
                           (cons value remaining-values) rest next-score))))))
        ((accept)
         (and (pair? semantic-values)
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
                                         states semantic-values rest score)))))
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
    (def (try-parse states semantic-values rest score)
      (let* ((configuration
              (intern-configuration states semantic-values rest score))
             (cached
              (table-ref configuration-results configuration
                         configuration-result-missing)))
        (cond
         ((eq? cached configuration-result-failed) #f)
         ((eq? cached configuration-result-visiting) #f)
         ((not (eq? cached configuration-result-missing))
          (set! configuration-memo-hits (fx+ configuration-memo-hits 1))
          (set! successful-completions
                (+ successful-completions
                   (candidate-completion-count cached)))
          cached)
         (else
          (table-set! configuration-results configuration
                      configuration-result-visiting)
          (let* ((states (glr-configuration-states configuration))
                 (semantic-values
                  (glr-configuration-semantic-values configuration))
                 (rest (glr-configuration-rest configuration))
                 (score (glr-configuration-score configuration))
                 (state (car states))
                 (action-row
                  (current-action-row
                   action-index state rest case-insensitive?))
                 (result
                  (if action-row
                    (try-action (cdr action-row) (car action-row)
                                states semantic-values rest score)
                    (begin
                      (record-failure! state rest)
                      #f))))
            (table-set! configuration-results configuration
                        (or result configuration-result-failed))
            result)))))
    ;; The prepared fast path hands its immutable checkpoint to selective GLR.
    ;; Starting from that checkpoint avoids replaying the deterministic prefix
    ;; from token zero whenever the first admitted fork is encountered.
    (let (result (try-parse initial-states initial-semantic-values
                           initial-rest 0))
      (unless result
        (error "input does not match LR parser"
               (or best-failure
                   '((failureKind . lr-no-complete-parse)
                     (byteOffset . 0)))))
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
        (cons 'internedConfigurations
              (value-interner-created-count configuration-interner))
        (cons 'configurationInternHits
              (value-interner-hit-count configuration-interner))
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

(def (lr-parse/prepared runtime tokens (observability #f))
  (let ((productions (lr-runtime-productions runtime))
        (table (lr-runtime-table runtime))
        (action-index (lr-runtime-action-index runtime))
        (goto-index (lr-runtime-goto-index runtime))
        (case-insensitive? (lr-runtime-case-insensitive? runtime))
        (input-end-offset
         (let loop ((rest tokens) (offset 0))
           (if (null? rest) offset
               (loop (cdr rest) (token-end (car rest)))))))
    (def (selective-glr-from-checkpoint states semantic-values rest
                                        deterministic-actions
                                        deterministic-shifts)
      (call-with-parser-observed-phase
       observability 'selective-glr-execution
       (lambda ()
         (lr-parse/prepared/receipt
          runtime tokens 256 states semantic-values rest
          deterministic-actions deterministic-shifts))))
    (let loop ((states '(0)) (semantic-values '()) (rest tokens)
               (deterministic-actions 0) (deterministic-shifts 0))
      (let* ((state (car states))
             (action-row
              (current-action-row action-index state rest case-insensitive?)))
        (if (not action-row)
          (let-values (((root remaining _receipt)
                        (selective-glr-from-checkpoint
                         states semantic-values rest deterministic-actions
                         deterministic-shifts)))
            (values root remaining))
          (let (action (cdr action-row))
            (case (car action)
              ((shift)
               (loop (cons (cadr action) states)
                     (cons (list (make-recognition-child #f (car rest)))
                           semantic-values)
                     (cdr rest)
                     (fx+ deterministic-actions 1)
                     (fx+ deterministic-shifts 1)))
              ((reduce)
               (let* ((production (vector-ref table (cadr action)))
                      (count (length (production-rhs production))))
                 (let-values (((popped-values remaining-values)
                               (split-at semantic-values count))
                              ((_popped-states remaining-states)
                               (split-at states count)))
                   (let* (
                      (offset (if (pair? rest) (token-start (car rest))
                                  input-end-offset))
                      (value
                       (reduce-value
                        production popped-values offset
                        make-recognition-fragment))
                      (target
                       (and (pair? remaining-states)
                            (goto-target
                             goto-index (car remaining-states)
                             (production-lhs production)))))
                 (if target
                   (loop (cons target remaining-states)
                         (cons value remaining-values) rest
                         (fx+ deterministic-actions 1)
                         deterministic-shifts)
                   (let-values (((root remaining _receipt)
                                 (selective-glr-from-checkpoint
                                  states semantic-values rest
                                  deterministic-actions
                                  deterministic-shifts)))
                     (values root remaining)))))))
              ((accept)
               (let (children
                     (and (pair? semantic-values)
                          (recognition-sequence->list
                           (car semantic-values))))
                 (if (and (pair? children)
                          (null? (cdr children))
                          (not (recognition-child-field (car children))))
                   (values (recognition-child-value (car children)) rest)
                   (let-values (((root remaining _receipt)
                                 (selective-glr-from-checkpoint
                                  states semantic-values rest
                                  deterministic-actions
                                  deterministic-shifts)))
                     (values root remaining)))))
              (else
               (let-values (((root remaining _receipt)
                             (selective-glr-from-checkpoint
                              states semantic-values rest
                              deterministic-actions deterministic-shifts)))
                 (values root remaining))))))))))

;; lr-parse
;; : (-> List List (Values Datum List))
(def (lr-parse spec tokens)
  (lr-parse/prepared (lr-prepare spec) tokens))
