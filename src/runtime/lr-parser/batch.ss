;;; -*- Gerbil -*-
;;; lr-parser: batch owner. Private execution state and named algorithms.

(import (only-in ../reduction-plan
                 reduce-lr-source-plan)
        (only-in ../lr-completion
                 make-candidate candidate-root candidate-rest candidate-score candidate-ambiguities candidate-winner-reason candidate-completion-count select-candidate)
        (only-in ../../compiler/lr
                 production-action layout-end-action? production-lhs production-precedence)
        (only-in ../recognition
                 make-recognition-child make-recognition-fragment recognition-child-value)
        (only-in ../funcs
                 make-value-interner current-recognition-sequence-fusion-enabled? recognition-sequence-root-child value-interner-created-count value-interner-hit-count value-interner-intern)
        (only-in ../layout
                 current-layout-frames layout-after-shift layout-after-end)
        (only-in ../token
                 token-end token-kind token-lexeme token-start)
        (only-in "./state.ss"
                 lr-runtime-productions lr-runtime-table lr-runtime-reduction-widths lr-runtime-actions lr-runtime-action-index lr-runtime-goto-index lr-runtime-case-insensitive? lr-runtime-layout? lr-runtime-reduction-plans lr-runtime-action-selector current-lr-branch-budget)
        (only-in "./prepare.ss"
                 lr-prepare goto-target)
        (only-in "./reduction.ss"
                 with-pop-reduction))
(export lr-parse/prepared/receipt
        lr-parse/prepared/receipt/materialized
        lr-parse/receipt)

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
         (select-action (lr-runtime-action-selector runtime))
         (goto-index (lr-runtime-goto-index runtime))
         (case-insensitive? (lr-runtime-case-insensitive? runtime))
         (layout? (lr-runtime-layout? runtime))
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
    (def (better-candidate current candidate)
      (let-values (((winner merged?) (select-candidate current candidate)))
        (when merged? (set! merge-count (+ merge-count 1)))
        winner))
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
                 (reduce-lr-source-plan
                  (vector-ref (lr-runtime-reduction-plans runtime) production-id)
                  source-values offset intern-fragment))
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
              (let (child (recognition-sequence-root-child (car semantic-values)))
                (and child
                     (let (root (recognition-child-value child))
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
              (select-action (car states) rest))
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
                    (select-action state rest))
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

