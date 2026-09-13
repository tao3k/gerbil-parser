;;; -*- Gerbil -*-
;;; Immutable LR table execution and lossless recognition reduction.

(import (only-in ../compiler/lr
                 lr-spec-ref operand-actions production-action
                 production-lhs production-precedence production-rhs
                 production-table)
        (only-in ./recognition
                 make-recognition-child recognition-child-field
                 recognition-child-value)
        (only-in ./reduce
                 recognition-children-alias recognition-children-field)
        (only-in ./token
                 token-end token-kind token-lexeme token-start))
(export lr-parse
        lr-parse/receipt
        lr-prepare
        lr-parse/prepared)

(def +lr-eof+ '(terminal eof))

;; Immutable execution data derived once for generated parser machines.
(defstruct lr-runtime
  (productions table actions gotos case-insensitive? dynamic?)
  transparent: #t)

(def (lr-prepare spec)
  (let* ((productions (lr-spec-ref spec 'productions))
         (dynamic?
          (let loop ((rest productions))
            (and (pair? rest)
                 (let (precedence (production-precedence (car rest)))
                   (or (and precedence (eq? (car precedence) 'dynamic))
                       (loop (cdr rest))))))))
    (make-lr-runtime
     productions
     (production-table productions)
     (lr-spec-ref spec 'actions)
     (lr-spec-ref spec 'gotos)
     (lr-spec-ref spec 'case-insensitive?)
     dynamic?)))

;; lookup-action-row
;; : (-> Vector Fixnum Datum (OrFalse Pair))
(def (lookup-action-row actions state terminal)
  (let loop ((rest (vector-ref actions state)))
    (and (pair? rest)
         (let (row (car rest))
           (if (equal? (car row) terminal)
             row
             (loop (cdr rest)))))))

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

;; take
;; : (-> List Fixnum List)
(def (take values count)
  (if (zero? count) '()
      (cons (car values) (take (cdr values) (- count 1)))))

;; drop
;; : (-> List Fixnum List)
(def (drop values count)
  (if (zero? count) values (drop (cdr values) (- count 1))))

;; apply-operand-action
;; : (-> List List Fixnum List)
(def (apply-operand-action action children default-offset)
  (case (car action)
    ((field)
     (recognition-children-field (cadr action) children default-offset))
    ((alias)
     (recognition-children-alias (cadr action) children default-offset))
    (else (error "unknown LR operand action" action))))

;; apply-operand-actions
;; : (-> List List Fixnum List)
(def (apply-operand-actions value actions default-offset)
  (foldl (lambda (action children)
           (apply-operand-action action children default-offset))
         value
         actions))

;; reduce-value
;; : (-> List List Fixnum List)
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

;; goto-target
;; : (-> Vector Fixnum Symbol (OrFalse Fixnum))
(def (goto-target gotos state lhs)
  (let (row (assq lhs (vector-ref gotos state)))
    (and row (cdr row))))

;; : (-> Datum List Integer List)
(def (make-candidate root rest score)
  (list root rest score))
(def candidate-root car)
(def candidate-rest cadr)
(def candidate-score caddr)

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
(def (lr-parse/prepared/receipt runtime tokens (branch-budget 256))
  (unless (and (integer? branch-budget) (positive? branch-budget))
    (error "selective GLR branch budget must be positive" branch-budget))
  (let* ((productions (lr-runtime-productions runtime))
         (table (lr-runtime-table runtime))
         (actions (lr-runtime-actions runtime))
         (gotos (lr-runtime-gotos runtime))
         (case-insensitive? (lr-runtime-case-insensitive? runtime))
         (dynamic? (lr-runtime-dynamic? runtime))
         (branches-explored 0)
         ;; The preferred action is the deterministic continuation of a fork.
         ;; Only fallback actions consume the speculative branch budget; a
         ;; long preferred path must not fail merely because it visits many
         ;; conflict cells.
         (speculative-branches-explored 0)
         (branch-identities '())
         (branch-sites '())
         (merge-count 0)
         (ambiguity-count 0)
         (budget-exhausted? #f)
         (best-failure #f))
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
        (if (pair? tokens) (token-end (car (reverse tokens))) 0)))
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
    (def (better-candidate current candidate)
      (cond
       ((not current) candidate)
       ((> (candidate-score candidate) (candidate-score current)) candidate)
       ((< (candidate-score candidate) (candidate-score current)) current)
       ((< (length (candidate-rest candidate))
           (length (candidate-rest current)))
        candidate)
       ((> (length (candidate-rest candidate))
           (length (candidate-rest current)))
        current)
       ((and (equal? (candidate-root candidate) (candidate-root current))
             (equal? (candidate-rest candidate) (candidate-rest current)))
        (set! merge-count (+ merge-count 1))
        current)
       (else
        ;; Stable v1 keeps declaration-order selection for distinct equal
        ;; scores while making the unresolved ambiguity observable.
        (set! ambiguity-count (+ ambiguity-count 1))
        current)))
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
                (count (length (production-rhs production)))
                (popped-values (take semantic-values count))
                (remaining-values (drop semantic-values count))
                (remaining-states (drop states count))
                (offset (if (pair? rest) (token-start (car rest))
                            (if (pair? tokens)
                              (token-end (car (reverse tokens))) 0)))
                (value (reduce-value production popped-values offset))
                (precedence (production-precedence production))
                (next-score
                 (if (and precedence (eq? (car precedence) 'dynamic))
                   (+ score (cadr precedence))
                   score))
                (target
                 (and (pair? remaining-states)
                      (goto-target gotos (car remaining-states)
                                   (production-lhs production)))))
           (and target
                (try-parse (cons target remaining-states)
                           (cons value remaining-values) rest next-score))))
        ((accept)
         (and (pair? semantic-values)
              (let (children (car semantic-values))
                (and (pair? children)
                     (null? (cdr children))
                     (not (recognition-child-field (car children)))
                     (make-candidate
                      (recognition-child-value (car children)) rest score)))))
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
                         (cons 'branchSites (reverse branch-sites)))))
               best)
             (begin
               (set! branches-explored (+ branches-explored 1))
               (unless preferred?
                 (set! speculative-branches-explored
                       (+ speculative-branches-explored 1)))
               (record-branch-site! (car states) terminal)
               (set! branch-identities
                     (cons branches-explored branch-identities))
               (when (> speculative-branches-explored branch-budget)
                 (set! budget-exhausted? #t))
               (let (candidate
                     (and (not budget-exhausted?)
                          (with-catch
                           (lambda (_) #f)
                           (lambda ()
                             (try-action (car branches) terminal
                                         states semantic-values rest score)))))
                 (if (and candidate (not dynamic?))
                   candidate
                   (loop (cdr branches)
                         (if candidate
                           (better-candidate best candidate)
                           best)
                         #f)))))))
        ((reject-nonassoc)
         (error "non-associative operator cannot be chained"
                (list
                 (cons 'failureKind 'non-associative-chain)
                 (cons 'precedence (cadr action))
                 (cons 'associativity (caddr action)))))
        (else (error "unknown LR action" action))))
    (def (try-parse states semantic-values rest score)
      (let* ((state (car states))
             (action-row
              (current-action-row actions state rest case-insensitive?)))
        (if action-row
          (try-action (cdr action-row) (car action-row)
                      states semantic-values rest score)
          (begin
            (record-failure! state rest)
            #f))))
    (let (result (try-parse '(0) '() tokens 0))
      (unless result
        (error "input does not match LR parser"
               (or best-failure
                   '((failureKind . lr-no-complete-parse)
                     (byteOffset . 0)))))
      (values
       (candidate-root result)
       (candidate-rest result)
       (list
        (cons 'schema "gerbil-parser.selective-glr-receipt.v1")
        (cons 'branchBudget branch-budget)
        (cons 'branchesExplored branches-explored)
        (cons 'speculativeBranchesExplored
              speculative-branches-explored)
        (cons 'branchIdentities (reverse branch-identities))
        (cons 'branchSites (reverse branch-sites))
        (cons 'mergedBranches merge-count)
        (cons 'ambiguousBranches ambiguity-count)
       (cons 'dynamicScore (candidate-score result)))))))

(def (lr-parse/receipt spec tokens (branch-budget 256))
  (lr-parse/prepared/receipt (lr-prepare spec) tokens branch-budget))

(def (lr-parse/prepared runtime tokens)
  (let ((productions (lr-runtime-productions runtime))
        (table (lr-runtime-table runtime))
        (actions (lr-runtime-actions runtime))
        (gotos (lr-runtime-gotos runtime))
        (case-insensitive? (lr-runtime-case-insensitive? runtime)))
    (let loop ((states '(0)) (semantic-values '()) (rest tokens))
      (let* ((state (car states))
             (action-row
              (current-action-row actions state rest case-insensitive?)))
        (if (not action-row)
          (let-values (((root remaining _receipt)
                        (lr-parse/prepared/receipt runtime tokens)))
            (values root remaining))
          (let (action (cdr action-row))
            (case (car action)
              ((shift)
               (loop (cons (cadr action) states)
                     (cons (list (make-recognition-child #f (car rest)))
                           semantic-values)
                     (cdr rest)))
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
                            (goto-target
                             gotos (car remaining-states)
                             (production-lhs production)))))
                 (if target
                   (loop (cons target remaining-states)
                         (cons value remaining-values) rest)
                   (let-values (((root remaining _receipt)
                                 (lr-parse/prepared/receipt runtime tokens)))
                     (values root remaining)))))
              ((accept)
               (let (children (and (pair? semantic-values)
                                   (car semantic-values)))
                 (if (and (pair? children)
                          (null? (cdr children))
                          (not (recognition-child-field (car children))))
                   (values (recognition-child-value (car children)) rest)
                   (let-values (((root remaining _receipt)
                                 (lr-parse/prepared/receipt runtime tokens)))
                     (values root remaining)))))
              (else
               (let-values (((root remaining _receipt)
                             (lr-parse/prepared/receipt runtime tokens)))
                 (values root remaining))))))))))

;; lr-parse
;; : (-> List List (Values Datum List))
(def (lr-parse spec tokens)
  (lr-parse/prepared (lr-prepare spec) tokens))
