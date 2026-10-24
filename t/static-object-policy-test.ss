(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure read-source-definition))

(deflanguage-parser-tests static-object-policy-test "owned static compiler host policy"
  (engine)
  (scenario "dependencies and test objects share the same per-procedure GCC policy"
    (scope ((options (source-procedure "src/build-support/compiler-profile.ss" 'compiler-c-options '())))
      (equal (options "-O1 -fwrapv") "-O1 -fwrapv -U___SINGLE_HOST -D___MULTIPLE_HOSTS")
      (equal (options "-O2 -Iinclude") "-O2 -Iinclude -U___SINGLE_HOST -D___MULTIPLE_HOSTS")))
  (scenario "clean preparation compiles source readers before dependent generators"
    (scope ((catalog (cadr (caddr (read-source-definition "build-language-conformance.ss" 'conformance-modules)))))
      (each (edge) ('(("t/fixtures/prediction-witness" "t/fixtures/event-walk-source")
                     ("t/fixtures/event-walk-source" "t/fixtures/lr1-construction")))
        (equal (and (member (car edge) catalog) (member (cadr edge) catalog)
                    (> (length (member (car edge) catalog)) (length (member (cadr edge) catalog)))) #t))))
  (scenario "every compiled conformance test owner has an explicit unified entry import"
    (scope ((catalog (cadr (caddr (read-source-definition "build-language-conformance.ss" 'conformance-modules))))
            (imports (source-procedure "scripts/test-plan.ss" 'conformance-imported-test-names '())))
      (each (owner) ((filter (lambda (name) (string-suffix? "-test" name)) catalog))
        (equal (pair? (imports owner)) #t))))
  (scenario "source index evaluation has an expander owner and native diagnostics stay compiled"
    (scope ((declared (cadr (caddr (read-source-definition "t/conformance-main.ss" 'source-suites))))
            (imports (source-procedure "scripts/test-plan.ss" 'conformance-imported-test-names '())))
      (equal (cdr (assoc "t/nullable-origin-test" declared)) '(nullable-index-source-test))
      (equal (imports "t/nullable-origin-test") '(nullable-origin-test)))))
