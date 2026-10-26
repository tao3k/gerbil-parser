;;; -*- Gerbil -*-
;;; lr-parser: prepare owner. Private execution state and named algorithms.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop validate)
        (only-in ../function-types ActionSelectorFactory)
        (only-in ../reduction-plan
                 prepare-lr-reduction-plans prepare-lr-operand-semantics RecognitionSemantics. EventSemantics.
                 resolve-lr-semantic-backend)
        (only-in ../../compiler/lr-table
                 validate-lr-tables layout-productions?)
        (only-in :std/vector/vector
                 vector-map/index)
        (only-in :yuanxu/poo-clos/interface
                 .defgeneric .defmethod)
        (only-in ../../compiler/lr
                 lr-spec-ref production-action validate-production-semantics production-precedence production-rhs production-table production-lhs)
        (only-in ../funcs
                 association-row-vector->index vector-intern-map)
        (only-in ../lr-action-index
                 index-action-rows lr-action-row-has-literals? lookup-literal-action-entry lr-action-row-eof lr-action-row-tokens lookup-casefolded-literal-action-entry)
        (only-in ../layout
                 prepare-layout-action-selector)
        (only-in ../token
                 token-kind token-lexeme)
        (only-in "./state.ss"
                 make-lr-runtime lr-runtime? lr-runtime-productions lr-runtime-table lr-runtime-reduction-widths lr-runtime-actions lr-runtime-action-index lr-runtime-gotos lr-runtime-goto-index lr-runtime-goto-columns lr-runtime-case-insensitive? lr-runtime-dynamic? lr-runtime-layout? lr-runtime-lexical-modes lr-runtime-lexical-mode-catalog lr-runtime-direct-step lr-runtime-direct-step-set! lr-runtime-semantic-reducer lr-runtime-event-reducer lr-runtime-event-step lr-runtime-event-step-set! lr-runtime-event-runtime lr-runtime-event-runtime-set! lr-runtime-reduction-plans lr-runtime-action-selector lr-runtime-owned-stack?-set! current-lr-event-program-enabled? make-lr-lexical-mode)
        (only-in "./reduction.ss"
                 reduce-value reduce-value/events))
(export prepare-lr-semantics
        install-lr-runtime-direct-step!
        select-lr-semantic-reducer select-lr-semantic-reducer$poo-clos-generic
        lr-semantic-capability
        lr-runtime-event-program?
        install-lr-runtime-event-step!
        lr-runtime-for-current-semantic-backend
        lr-prepare
        prepare-token-action-lookup
        make-lr-action-selector
        lr-runtime-with-action-selector
        prepare-lr-goto-columns
        goto-target)

;;; Install a generated reduction step once, before the runtime is shared.
(def (install-lr-runtime-direct-step! runtime step)
  (unless (and (lr-runtime? runtime)
               (procedure? step)
               (not (lr-runtime-direct-step runtime)))
    (error "invalid generated LR reduction step"))
  (lr-runtime-direct-step-set! runtime step)
  (lr-runtime-owned-stack?-set! runtime #f)
  (lr-runtime-event-runtime-set! runtime #f))

;;; Preparation resolves native prototype methods along two independent axes.
;;; Runtime requests keep the admitted callables; method contributions affect
;;; future preparation and never cause request-time generic redispatch.
(.defgeneric select-lr-semantic-reducer (backend capability))

(.defmethod (select-lr-semantic-reducer (backend (prototype RecognitionSemantics.)) capability)
  reduce-value)

(.defmethod (select-lr-semantic-reducer (backend (prototype EventSemantics.)) capability)
  #f)

(.defmethod (select-lr-semantic-reducer (backend (prototype EventSemantics.))
                                          (capability (eql 'plain)))
  reduce-value/events)

;; Existing symbol callers delegate through the same native families.
(.defmethod (select-lr-semantic-reducer (backend (eql 'recognition)) capability)
  (select-lr-semantic-reducer RecognitionSemantics. capability))
(.defmethod (select-lr-semantic-reducer (backend (eql 'event)) capability)
  (select-lr-semantic-reducer EventSemantics. capability))

;;; A selection is the explicit result of method selection. Grammar lowering
;;; reuses it without re-entering MOP dispatch; callers request a new selection
;;; after contributing methods. The native carrier is private engine state.
(defstruct lr-prepared-semantics (backend operands reducers) final: #t)
(def (capture-lr-reducers backend)
  (let ((selected-plain (select-lr-semantic-reducer backend 'plain))
        (selected-general (select-lr-semantic-reducer backend 'general))
        (selected-dynamic (select-lr-semantic-reducer backend 'dynamic))
        (selected-branching (select-lr-semantic-reducer backend 'branching))
        (selected-layout (select-lr-semantic-reducer backend 'layout)))
    (.o plain: selected-plain general: selected-general dynamic: selected-dynamic branching: selected-branching layout: selected-layout)))
(.defgeneric prepare-lr-recognition-semantics (backend))
(.defmethod (prepare-lr-recognition-semantics
              (backend (prototype RecognitionSemantics.)))
  (make-lr-prepared-semantics backend (prepare-lr-operand-semantics backend)
                            (capture-lr-reducers backend)))
(def (prepare-lr-semantics backend)
  (if (lr-prepared-semantics? backend) backend
    (prepare-lr-recognition-semantics (resolve-lr-semantic-backend backend))))
;; Builtin Library publication selects its native selections once. Other Library
;; objects use the same public preparation operation, without editing a catalog.
(def default-recognition-semantics (prepare-lr-semantics RecognitionSemantics.))
(def default-event-reducers (capture-lr-reducers EventSemantics.))

;;; Grammar eligibility belongs to preparation, including negative admission.
;;; It is independent of the request's optional backend preference.
(def (lr-semantic-capability table actions dynamic? layout?)
  (cond
   (layout? 'layout)
   (dynamic? 'dynamic)
   ((vector-any (lambda (row)
                  (any (lambda (entry) (eq? (cadr entry) 'fork)) row)) actions)
    'branching)
   ((vector-any (lambda (production)
                  (not (memq (production-action production) '(pass concat)))) table)
    'general)
   (else 'plain)))

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
   ((not (lr-runtime-event-reducer runtime)) runtime)
   (else
    (let (selected
          (make-lr-runtime
           (lr-runtime-productions runtime) (lr-runtime-table runtime)
           (lr-runtime-reduction-widths runtime) (lr-runtime-actions runtime)
           (lr-runtime-action-index runtime) (lr-runtime-gotos runtime)
           (lr-runtime-goto-index runtime) (lr-runtime-case-insensitive? runtime)
           #f #f (lr-runtime-lexical-modes runtime)
           (lr-runtime-lexical-mode-catalog runtime)
           (lr-runtime-event-step runtime) (lr-runtime-event-reducer runtime) #f #f #f
           (prepare-lr-reduction-plans 'event (lr-runtime-table runtime))
           (lr-runtime-action-selector runtime) #f (lr-runtime-goto-columns runtime) (void)))
      (lr-runtime-event-runtime-set! runtime selected)
      selected))))

(def (lr-prepare spec (semantic-backend default-recognition-semantics))
  (let* ((productions (validate-production-semantics (lr-spec-ref spec 'productions) (lr-spec-ref spec 'declarations)))
         (actions (lr-spec-ref spec 'actions))
         (gotos (lr-spec-ref spec 'gotos))
         (dynamic?
          (any (lambda (production)
                 (let (precedence (production-precedence production))
                   (and precedence (eq? (car precedence) 'dynamic))))
               productions))
         (layout? (layout-productions? productions)))
    (validate-lr-tables productions actions gotos)
    (let-values (((modes mode-catalog)
                  (vector-intern-map
                   actions
                   (lambda (row) (map car row))
                   (lambda (terminals id)
                     (make-lr-lexical-mode id terminals)))))
      (let* ((semantics (prepare-lr-semantics semantic-backend))
             (backend (lr-prepared-semantics-backend semantics))
             (table (production-table productions))
             (capability (lr-semantic-capability table actions dynamic? layout?))
             (case-insensitive? (lr-spec-ref spec 'case-insensitive?))
             (action-index (index-action-rows actions case-insensitive?))
             (goto-index (association-row-vector->index gotos)))
        (let ((plans (prepare-lr-reduction-plans (lr-prepared-semantics-operands semantics) table))
              (reducer (.ref (lr-prepared-semantics-reducers semantics) capability)))
        (make-lr-runtime
         productions
         table
         (vector-map/index
          (lambda (_index production)
            (length (production-rhs production))) table)
         actions
         action-index
         gotos
         goto-index
         (lr-spec-ref spec 'case-insensitive?)
         dynamic?
         layout?
         modes
         mode-catalog
         #f reducer
         ;; An inherited/custom recognition backend has not supplied a matching
         ;; event interpretation. Preserve its semantics instead of substituting
         ;; the builtin event family when the optional preference is enabled.
         (and (eq? backend RecognitionSemantics.)
              (.ref default-event-reducers capability)) #f #f plans
         (make-lr-action-selector actions action-index case-insensitive? layout?)
         (not layout?) (prepare-lr-goto-columns table goto-index) (void)))))))

;;; Token terminals are admitted symbols. Specialize the owned row shape once;
;;; first duplicate entries and exact action-pair identity remain unchanged.
(def (prepare-token-action-lookup entries)
  (cond
   ((null? entries) (lambda (_kind) #f))
   ((and (pair? entries) (null? (cdr entries)))
    (let ((kind (caar entries)) (entry (cdar entries)))
      (lambda (candidate) (and (eq? kind candidate) entry))))
   ((pair? entries)
    (lambda (kind) (let (entry (assq kind entries)) (and entry (cdr entry)))))
   (else (lambda (kind) (table-ref entries kind #f)))))

(def (make-lr-action-selector rows index case-insensitive? layout?)
  (if layout?
    (prepare-layout-action-selector rows index case-insensitive?)
    (let (selectors
          (vector-map/index
           (lambda (_state row)
             (let ((eof (lr-action-row-eof row)) (lookup-token (prepare-token-action-lookup (lr-action-row-tokens row))))
               (if (lr-action-row-has-literals? row)
                 (lambda (input)
                   (if (not input) eof
                     (let (token input)
                       (or (lookup-literal-action-entry row (token-lexeme token))
                           (and case-insensitive?
                             (lookup-casefolded-literal-action-entry row (token-lexeme token)))
                           (lookup-token (token-kind token))))))
                 (lambda (input)
                   (if (not input) eof
                     (lookup-token (token-kind input))))))) index))
      (lambda (state input) ((vector-ref selectors state) input)))))

;;; Qualification/extension preparation replaces only the action lookup
;;; primitive. Immutable grammar tables, semantic plans and modes stay owned.
;;; Generated steps cannot bypass the supplied selection procedure.
(def (lr-runtime-with-action-selector runtime factory)
  ;; Native Function validates the returned selector recursively. The prepared
  ;; runtime retains it; generated direct steps cannot bypass the replacement.
  (let (selector ((validate ActionSelectorFactory factory)
                 (lr-runtime-actions runtime) (lr-runtime-action-index runtime)
                 (lr-runtime-case-insensitive? runtime) (lr-runtime-layout? runtime)))
    (make-lr-runtime
     (lr-runtime-productions runtime) (lr-runtime-table runtime)
     (lr-runtime-reduction-widths runtime) (lr-runtime-actions runtime)
     (lr-runtime-action-index runtime) (lr-runtime-gotos runtime) (lr-runtime-goto-index runtime)
     (lr-runtime-case-insensitive? runtime) (lr-runtime-dynamic? runtime) (lr-runtime-layout? runtime)
     (lr-runtime-lexical-modes runtime) (lr-runtime-lexical-mode-catalog runtime)
     #f (lr-runtime-semantic-reducer runtime) (lr-runtime-event-reducer runtime) #f #f
     (lr-runtime-reduction-plans runtime) selector
     (and (not (lr-runtime-layout? runtime))
          (not (lr-runtime-event-program? runtime)))
     (lr-runtime-goto-columns runtime) (void))))

;; goto-target
;; : (-> (Vector (Or (List Pair) HashTable)) Fixnum Symbol (OrFalse Fixnum))
;;; Canonical production admission requires symbolic nonterminals. Stage the
;;; private lookup into each transition: symbol equality is identity equality,
;;; and the original first entry remains authoritative. General association
;;; indexes retain their equal?-keyed contract.
(defrule (goto-target gotos-expr state-expr lhs-expr)
  (let* ((gotos gotos-expr) (state state-expr) (lhs lhs-expr)
         (index (vector-ref gotos state))
         (entry (if (or (pair? index) (null? index))
                  (assq lhs index) (hash-get index lhs))))
    (and entry (cdr entry))))


;;; Preparation owns these columns. Share each LHS column across productions;
;;; cap retained cells before allocation and keep sparse rows above the budget.
(def (prepare-lr-goto-columns table gotos (cell-budget 65536))
  (unless (and (exact-integer? cell-budget) (>= cell-budget 0))
    (error "goto column cell budget must be a nonnegative integer"))
  (let ((by-lhs (make-table test: eq?)) (keys '()))
    (vector-for-each
      (lambda (production)
        (let (lhs (production-lhs production))
          (unless (table-ref by-lhs lhs #f)
            (table-set! by-lhs lhs #t)
            (set! keys (cons lhs keys))))) table)
    (and (<= (* (length keys) (vector-length gotos)) cell-budget)
      (begin
        (for-each
          (lambda (lhs)
            (let (column (make-vector (vector-length gotos) #f))
              (let loop ((state 0))
                (when (< state (vector-length gotos))
                  (vector-set! column state (goto-target gotos state lhs))
                  (loop (+ state 1))))
              (table-set! by-lhs lhs column))) keys)
        (vector-map/index (lambda (_id production) (table-ref by-lhs (production-lhs production))) table)))))
