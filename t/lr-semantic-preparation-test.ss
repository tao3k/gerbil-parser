(import :gerbil-parser/language-test-support)
;;; Prepared backend admission and retained runtime identity.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-runtime-for-current-semantic-backend
                 current-lr-event-program-enabled? lr-runtime-event-program?
                 lr-runtime-direct-step install-lr-runtime-event-step!))
(import (only-in :gerbil-parser/src/runtime/lr-parser/state
                 lr-runtime-productions lr-runtime-table lr-runtime-reduction-widths
                 lr-runtime-actions lr-runtime-action-index lr-runtime-gotos lr-runtime-goto-index
                 lr-runtime-case-insensitive? lr-runtime-dynamic? lr-runtime-layout?
                 lr-runtime-lexical-modes lr-runtime-lexical-mode-catalog lr-runtime-action-selector
                 lr-runtime-goto-columns))


;;; Recognition kernel is shared; backend-specific reducers/plans are not.
(defparser-check (check-recognition-kernel source selected)
  (each (field) ((list lr-runtime-productions lr-runtime-table lr-runtime-reduction-widths
         lr-runtime-actions lr-runtime-action-index lr-runtime-gotos lr-runtime-goto-index
         lr-runtime-case-insensitive? lr-runtime-lexical-modes lr-runtime-lexical-mode-catalog
         lr-runtime-action-selector lr-runtime-goto-columns)) (equal (eq? (field source) (field selected))  #t))
  (equal (lr-runtime-dynamic? selected)  #f)
  (equal (lr-runtime-layout? selected)  #f))

(def (event-runtime runtime)
  (parameterize ((current-lr-event-program-enabled? #t))
    (lr-runtime-for-current-semantic-backend runtime)))

(deflanguage-parser-tests lr-semantic-preparation-test "LR semantic preparation"
  (engine)
    (scenario "preparation captures both strategies independently of request preference"
      (sequential ((spec (compile-lr-spec '((source-file (token word))) 'source-file))
             (runtime (parameterize ((current-lr-event-program-enabled? #t))
                        (lr-prepare spec)))
             (selected (event-runtime runtime)))
        (equal (lr-runtime-event-program? runtime)  #f)
        (equal (lr-runtime-event-program? selected)  #t)
        (equal (eq? selected runtime)  #f)
        (verify check-recognition-kernel runtime selected)
        (verify check-recognition-kernel runtime (event-runtime runtime))
        (verify check-recognition-kernel runtime (event-runtime selected))
        (equal (eq? selected (event-runtime runtime))  #t)
        (equal (eq? selected (event-runtime selected))  #t)
        (parameters ((current-lr-event-program-enabled? #f))
          (equal (eq? runtime (lr-runtime-for-current-semantic-backend runtime))  #t)
          (equal (eq? selected (lr-runtime-for-current-semantic-backend selected))  #t))
        (equal (eq? selected (event-runtime (lr-prepare spec)))  #f)))
    (scenario "generated step installation changes future selection, not retained executors"
      (sequential ((runtime (lr-prepare (compile-lr-spec '((source-file (token word))) 'source-file)))
             (retained (event-runtime runtime))
             (step (lambda args (error "test step must not execute"))))
        (act (install-lr-runtime-event-step! runtime step))
        (scope ((selected (event-runtime runtime)))
          (equal (eq? selected retained)  #f)
          (verify check-recognition-kernel runtime retained)
          (verify check-recognition-kernel runtime selected)
          (equal (eq? selected (event-runtime runtime))  #t)
          (equal (eq? step (lr-runtime-direct-step selected))  #t)
          (equal (lr-runtime-direct-step retained)  #f))
        (raises (install-lr-runtime-event-step! runtime step) true)))
    (scenario "dynamic, branching and layout grammars retain recognition fallback"
      (each (rules) ('(((source-file (precedence dynamic 1 (token word))))
         ((source-file (choice (reference first-path) (reference second-path)))
          (first-path (alias First (token word)))
          (second-path (alias Second (token word))))
         ((source-file (sequence (layout-start "|>") (token word) (layout-end "END"))))))
         (scope ((runtime (lr-prepare (compile-lr-spec rules 'source-file 'selective-glr))))
           (each (_) ('(first second third)) (equal (eq? runtime (event-runtime runtime))  #t))
           (equal (lr-runtime-event-program? runtime)  #f)))))
