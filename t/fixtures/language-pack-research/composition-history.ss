#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; The existing normalizer remains the sole owner of composition behavior.
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)
        "list-roles" "list-origins"
        (only-in "../../../src/modules/parser/objects" make-grammar-role make-grammar grammar-role-ref)
        (only-in "../../../src/compiler/normalize" compile-grammar compile-grammar/receipt grammar-ir-ref
                 compile-grammar/context normalized-grammar-ir normalized-grammar-receipt
                 normalized-grammar-source-map)
        (only-in "../../../src/compiler/parser-ir" compile-parser parser-ir-ref)
        (only-in "../../../src/compiler/language-artifact" project-language-catalog)
        (only-in "../../../src/compiler/bound-ir" bind-grammar-ir bound-grammar-ir-binding))
(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "composition history mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "HISTORY-CASE-OK: ") (display label) (newline) (force-output))
(def (ref value key) (let (row (assq key value)) (and row (cdr row))))
(def (rule-role owner rows)
  (make-grammar-role owner '() '() '() rows '() '() '() '() '()))
(def base (make-list-study-grammar))
(def replacement '(field argument (reference name)))
(def override-role (rule-role 'single-argument-extension (list (list 'arguments replacement))))
(def remove-role (rule-role 'retired-tail '((call-arguments/tail (empty)))))
(def (error-receipt thunk)
  (with-catch (lambda (condition) (list (error-message condition) (error-irritants condition))) thunk))

(let-values (((grammar receipt)
              (compile-grammar/receipt
               (make-grammar 'overridden (list base) '()
                 (list (cons 'override override-role))))))
  (probe 'override-replaces-rule (assq 'arguments (grammar-ir-ref grammar 'rules))
    (list 'arguments replacement))
  (probe 'override-retains-independent-helper
    (and (assq 'call-arguments/tail (grammar-ir-ref grammar 'rules)) #t) #t)
  (probe 'receipt-retains-operation-order
    (map (lambda (step) (ref step 'operation)) (ref receipt 'steps))
    '(merge append append override))
  (probe 'receipt-retains-owner-history
    (map (lambda (step) (ref step 'role)) (ref receipt 'steps))
    '(list-study-base-role call-arguments array-elements single-argument-extension))
  (let* ((append-only-map (list-study-source-map receipt))
         (bound (bind-grammar-ir grammar "history-fixture" '(composition-history) append-only-map)))
    ;; Retain the negative control: this previous fixture only supports unique append.
    (probe 'append-only-origin-is-stale-after-override
      (ref (ref (bound-grammar-ir-binding bound 'rule 'arguments) 'source) 'componentOwner)
      'call-arguments))
  ;; Explicit selected provenance for this one row; not an automatic history resolver.
  (let* ((source
          '((path . "t/fixtures/language-pack-research/composition-history.ss")
            (location . "single-argument-extension: arguments override")
            (generated? . #t)
            (componentOwner . single-argument-extension)
            (contributionHistory . ((1 append call-arguments) (3 override single-argument-extension)))))
         (bound (bind-grammar-ir grammar "history-fixture" '(composition-history)
                  (list (cons 'rule (list (cons 'arguments source)))))))
    (probe 'selected-override-owner
      (ref (ref (bound-grammar-ir-binding bound 'rule 'arguments) 'source) 'componentOwner)
      'single-argument-extension)
    (probe 'retained-explicit-history
      (ref (ref (bound-grammar-ir-binding bound 'rule 'arguments) 'source) 'contributionHistory)
      '((1 append call-arguments) (3 override single-argument-extension))))
  (probe 'overridden-parser-admitted (parser-ir-ref (compile-parser grammar) 'root-rule) 'source-file))

(let ((dangling
       (compile-grammar
        (make-grammar 'dangling (list base) '() (list (cons 'remove remove-role))))))
  (probe 'normalizer-removes-helper
    (assq 'call-arguments/tail (grammar-ir-ref dangling 'rules)) #f)
  (probe 'parser-rejects-dangling-helper
    (error-receipt (lambda () (compile-parser dangling)))
    '("unresolved grammar identity" (arguments call-arguments/tail)))
  (let (condition (error-receipt
                  (lambda () (bind-grammar-ir dangling "history-fixture" '(composition-history)))))
    (probe 'bound-ir-rejects-dangling-helper (car condition) "unresolved bound grammar reference")
    (probe 'bound-diagnostic-target
      (ref (car (cadr condition)) 'name) 'call-arguments/tail)))

(let-values (((closed receipt)
              (compile-grammar/receipt
               (make-grammar 'closed (list base) '()
                 (list (cons 'override override-role) (cons 'remove remove-role))))))
  (probe 'closed-replacement (assq 'arguments (grammar-ir-ref closed 'rules))
    (list 'arguments replacement))
  (probe 'closed-helper-absent (assq 'call-arguments/tail (grammar-ir-ref closed 'rules)) #f)
  (probe 'closed-parser-admitted (parser-ir-ref (compile-parser closed) 'root-rule) 'source-file)
  (probe 'closed-bound-helper-absent
    (bound-grammar-ir-binding
     (bind-grammar-ir closed "history-fixture" '(composition-history)) 'rule 'call-arguments/tail) #f)
  (probe 'receipt-retains-removal-after-override
    (map (lambda (step) (ref step 'operation)) (ref receipt 'steps))
    '(merge append append override remove)))

(probe 'override-missing-target
  (car (error-receipt
        (lambda () (compile-grammar
                    (make-grammar 'missing '() '() (list (cons 'override override-role)))))))
  "grammar override target does not exist")
(probe 'remove-missing-target
  (car (error-receipt
        (lambda () (compile-grammar
                    (make-grammar 'missing '() '() (list (cons 'remove remove-role)))))))
  "grammar remove target does not exist")
;;; Engine study: canonical POO catalogs are explicit role data, unlike concise DSL inference.
(let* ((parameters (make-nonempty-list-role 'call-arguments 'arguments
                     '(reference name) '(literal ",") 'parameter))
       (rule-only (make-grammar-role 'parameter-extension '() '() '()
                   (grammar-role-ref parameters 'rules) '() '() '() '() '()))
       (catalog-and-rules
        (make-grammar-role 'parameter-extension
          '((Call node (argument callee parameter))) '() '()
          (grammar-role-ref parameters 'rules) '() '() '() '() '()))
       (rules-grammar
        (compile-grammar
         (make-grammar 'field-contract (list base) '()
           (list (cons 'override rule-only)))))
       (complete-grammar
        (compile-grammar
         (make-grammar 'field-contract (list base) '()
           (list (cons 'override catalog-and-rules)))))
       (plain-bound (bind-grammar-ir rules-grammar "field-study" '(composition-history)))
       (source
        '((path . "t/fixtures/language-pack-research/composition-history.ss")
          (location . "parameter-extension: selected rows")
          (generated? . #t) (componentOwner . parameter-extension)))
       (selected-map
        (list (cons 'rule (list (cons 'arguments source)
                               (cons 'call-arguments/tail source)))))
       (complete-bound
        (bind-grammar-ir complete-grammar "field-study" '(composition-history) selected-map)))
  (probe 'rule-only-override-preserves-explicit-catalog
    (assq 'Call (grammar-ir-ref rules-grammar 'syntax-kinds))
    '(Call node (callee argument)))
  (probe 'rule-only-entry-emits-parameter
    (assq 'arguments (grammar-ir-ref rules-grammar 'rules))
    '(arguments (sequence (field parameter (reference name)) (reference call-arguments/tail))))
  (probe 'rule-only-tail-emits-parameter
    (assq 'call-arguments/tail (grammar-ir-ref rules-grammar 'rules))
    '(call-arguments/tail (repeat (sequence (literal ",") (field parameter (reference name))))))
  (probe 'canonical-compiler-keeps-supplied-catalog
    (assq 'Call (parser-ir-ref (compile-parser rules-grammar) 'syntax-kinds))
    '(Call node (callee argument)))
  (probe 'bound-fields-follow-supplied-catalog
    (bound-grammar-ir-binding plain-bound 'field 'parameter 'Call) #f)
  (probe 'joint-override-retains-declared-optional-field
    (assq 'Call (grammar-ir-ref complete-grammar 'syntax-kinds))
    '(Call node (argument callee parameter)))
  (probe 'joint-override-keeps-same-effective-rules
    (grammar-ir-ref complete-grammar 'rules) (grammar-ir-ref rules-grammar 'rules))
  (probe 'joint-override-binds-parameter-field
    (and (bound-grammar-ir-binding complete-bound 'field 'parameter 'Call) #t) #t)
  (probe 'joint-override-binds-retained-argument-field
    (and (bound-grammar-ir-binding complete-bound 'field 'argument 'Call) #t) #t)
  (probe 'joint-override-parser-retains-effective-catalog
    (assq 'Call (parser-ir-ref (compile-parser complete-grammar) 'syntax-kinds))
    '(Call node (argument callee parameter)))
  ;; Projection controls use explicit known catalogs, not a new field inference pass.
  (let* ((retained-kinds (grammar-ir-ref complete-grammar 'syntax-kinds))
         (inferred-kinds
          (map (lambda (row)
                 (if (eq? (car row) 'Call) '(Call node (callee parameter)) row))
               retained-kinds))
         (terminals (grammar-ir-ref complete-grammar 'terminals))
         (retained-catalog
          (list 'catalog (cons 'syntax-kinds retained-kinds) (cons 'terminals terminals)))
         (minimal-catalog
          (list 'catalog (cons 'syntax-kinds inferred-kinds) (cons 'terminals terminals)))
         (stale-catalog
          (list 'catalog (cons 'syntax-kinds (grammar-ir-ref rules-grammar 'syntax-kinds))
                (cons 'terminals terminals))))
    (let-values (((kinds mapped) (project-language-catalog inferred-kinds terminals retained-catalog)))
      (probe 'projection-admits-retained-optional-field
        (assq 'Call kinds) '(Call node (argument callee parameter))))
    (probe 'projection-rejects-stale-emitted-field-catalog
      (car (error-receipt
            (lambda () (project-language-catalog inferred-kinds terminals stale-catalog))))
      "catalog must retain every inferred kind, category and field")
    (probe 'projection-preserves-explicit-retained-field-obligation
      (car (error-receipt
            (lambda () (project-language-catalog retained-kinds terminals minimal-catalog))))
      "catalog must retain every inferred kind, category and field")
    (probe 'projection-rejects-terminal-remapping
      (car (error-receipt
            (lambda ()
              (project-language-catalog inferred-kinds terminals
                (list 'catalog (cons 'syntax-kinds retained-kinds)
                      (cons 'terminals
                        (map (lambda (row)
                               (if (eq? (car row) 'identifier)
                                 '(identifier Punctuation) row)) terminals)))))))
      "catalog must retain the exact inferred terminal mapping")
    (probe 'projection-rejects-duplicate-fields
      (car (error-receipt
            (lambda ()
              (project-language-catalog inferred-kinds terminals
                (list 'catalog
                      (cons 'syntax-kinds
                        (map (lambda (row)
                               (if (eq? (car row) 'Call)
                                 '(Call node (callee parameter parameter)) row)) retained-kinds))
                      (cons 'terminals terminals))))))
      "catalog has an invalid syntax-kind row")
    (let-values (((kinds mapped) (project-language-catalog inferred-kinds terminals #f)))
      (probe 'projection-without-explicit-catalog-preserves-inputs
        (list kinds mapped) (list inferred-kinds terminals))))
  (for-each
   (lambda (name)
     (probe (list 'selected-source-for-effective-row name)
       (ref (ref (bound-grammar-ir-binding complete-bound 'rule name) 'source) 'componentOwner)
       'parameter-extension))
   '(arguments call-arguments/tail)))
;;; Engine selection controls retain the manual and stale-map controls above.
(def (occurrence-source role index section row)
  (list (cons 'path "composition-occurrence-fixture")
        (cons 'location (number->string index))
        (cons 'generated? #t)
        ;; An accidental stale owner in supplied metadata cannot win selection.
        (cons 'componentOwner 'stale-supplied-owner)))
(def (selected-source normalized namespace name)
  (ref (ref (normalized-grammar-source-map normalized "fallback-origin") namespace) name))
(let* ((composed
        (make-grammar 'closed (list base) '()
          (list (cons 'override override-role) (cons 'remove remove-role))))
       (normalized (compile-grammar/context composed occurrence-source))
       (grammar (normalized-grammar-ir normalized))
       (sources (normalized-grammar-source-map normalized "fallback-origin"))
       (bound (bind-grammar-ir grammar "fallback-origin" '(composition-history) sources))
       (selected (selected-source normalized 'rule 'arguments)))
  (let-values (((plain receipt) (compile-grammar/receipt composed)))
    (probe 'context-capture-preserves-complete-ir grammar plain)
    (probe 'context-capture-preserves-complete-receipt
      (normalized-grammar-receipt normalized) receipt))
  (probe 'automatic-override-owner (ref selected 'componentOwner) 'single-argument-extension)
  (probe 'automatic-override-step (ref selected 'compositionStep) 3)
  (probe 'automatic-override-coordinates (ref selected 'location) "3")
  (probe 'automatic-override-history (ref selected 'contributionHistory)
    '((1 append call-arguments) (3 override single-argument-extension)))
  (probe 'automatic-remove-has-no-effective-source
    (selected-source normalized 'rule 'call-arguments/tail) #f)
  (probe 'automatic-remove-has-no-bound-binding
    (bound-grammar-ir-binding bound 'rule 'call-arguments/tail) #f)
  (probe 'automatic-bound-source-equals-selected-source
    (ref (bound-grammar-ir-binding bound 'rule 'arguments) 'source) selected)
  (probe 'unmodified-field-follows-effective-kind
    (ref (ref (bound-grammar-ir-binding bound 'field 'argument 'Call) 'source) 'componentOwner)
    'list-study-base-role))
(let* ((first (rule-role 'same-owner '((x (empty)))))
       (duplicate (rule-role 'same-owner '((x (empty)))))
       (replacement (rule-role 'same-owner '((x (literal "x")))))
       (removed (rule-role 'same-owner '((x (empty)))))
       (merged (compile-grammar/context
                (make-grammar 'same-name '() (list first duplicate)) occurrence-source))
       (overridden (compile-grammar/context
                    (make-grammar 'same-name '() (list first duplicate)
                      (list (cons 'override replacement))) occurrence-source))
       (reappended (compile-grammar/context
                    (make-grammar 'same-name '() (list first)
                      (list (cons 'remove removed) (cons 'append replacement))) occurrence-source)))
  (probe 'equal-merge-keeps-first-step
    (ref (selected-source merged 'rule 'x) 'compositionStep) 0)
  (probe 'equal-merge-keeps-first-source
    (ref (selected-source merged 'rule 'x) 'location) "0")
  (probe 'equal-merge-retains-accepted-history
    (ref (selected-source merged 'rule 'x) 'contributionHistory)
    '((0 merge same-owner) (1 merge same-owner)))
  (probe 'same-name-override-selects-occurrence
    (ref (selected-source overridden 'rule 'x) 'compositionStep) 2)
  (probe 'same-name-override-selects-reader-association
    (ref (selected-source overridden 'rule 'x) 'location) "2")
  (probe 'remove-reappend-selects-new-occurrence
    (ref (selected-source reappended 'rule 'x) 'compositionStep) 2)
  (probe 'remove-reappend-starts-effective-history
    (ref (selected-source reappended 'rule 'x) 'contributionHistory) '((2 append same-owner)))
  (probe 'remove-reappend-retains-full-receipt-history
    (map (lambda (step) (ref step 'operation))
         (ref (normalized-grammar-receipt reappended) 'steps)) '(merge remove append))
  (let* ((unknown (compile-grammar/context (make-grammar 'unknown '() (list first))))
         (source (selected-source unknown 'rule 'x)))
    (probe 'missing-reader-coordinates-use-origin (ref source 'location) "fallback-origin")
    (probe 'missing-reader-association-does-not-invent-generated-status (assq 'generated? source) #f)))
(let* ((parameters (make-nonempty-list-role 'call-arguments 'arguments
                     '(reference name) '(literal ",") 'parameter))
       (catalog-and-rules
        (make-grammar-role 'parameter-extension '((Call node (argument callee parameter)))
          '() '() (grammar-role-ref parameters 'rules) '() '() '() '() '()))
       (normalized (compile-grammar/context
                    (make-grammar 'field-selection (list base) '()
                      (list (cons 'override catalog-and-rules))) occurrence-source))
       (sources (normalized-grammar-source-map normalized "field-selection"))
       (bound (bind-grammar-ir (normalized-grammar-ir normalized) "field-selection"
                              '(composition-history) sources)))
  (probe 'joint-override-field-source-follows-kind
    (ref (ref (bound-grammar-ir-binding bound 'field 'parameter 'Call) 'source) 'componentOwner)
    'parameter-extension)
  (probe 'retained-field-source-follows-selected-catalog
    (ref (ref (bound-grammar-ir-binding bound 'field 'argument 'Call) 'source) 'compositionStep) 3)
  (probe 'joint-override-kind-and-field-share-source
    (ref (bound-grammar-ir-binding bound 'syntax-kind 'Call) 'source)
    (ref (bound-grammar-ir-binding bound 'field 'parameter 'Call) 'source)))

(let ((calls 0))
  (probe 'invalid-operation-still-rejected
    (car (error-receipt
          (lambda ()
            (compile-grammar/context
             (make-grammar 'invalid '() '() (list (cons 'override override-role)))
             (lambda args (set! calls (+ calls 1)) '())))))
    "grammar override target does not exist")
  (probe 'rejected-occurrence-does-not-read-source calls 0))
(probe 'malformed-source-rejected
  (car (error-receipt
        (lambda () (compile-grammar/context base (lambda args 'not-a-source-map)))))
  "invalid grammar occurrence source")

(display "COMPOSITION-HISTORY-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(test-child-process-exit! 0)
