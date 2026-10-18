(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure read-source-definition))

(deflanguage-parser-tests static-object-policy-test "owned static compiler host policy"
  (engine)
  (scenario "dependencies and test objects share the same per-procedure GCC policy"
    (scope ((options (source-procedure "build-language-conformance-link.ss" 'static-object-options '())))
      (equal (options "-O1 -fwrapv") "-O1 -fwrapv -U___SINGLE_HOST -D___MULTIPLE_HOSTS")
      (equal (options "-O2 -Iinclude") "-O2 -Iinclude -U___SINGLE_HOST -D___MULTIPLE_HOSTS")))
  (scenario "clean preparation compiles source readers before dependent generators"
    (scope ((catalog (cadr (caddr (read-source-definition "build-language-conformance.ss" 'conformance-modules)))))
      (each (edge) ('(("t/fixtures/prediction-witness" "t/fixtures/event-walk-source")
                     ("t/fixtures/event-walk-source" "t/fixtures/lr1-construction")))
        (equal (and (member (car edge) catalog) (member (cadr edge) catalog)
                    (> (length (member (car edge) catalog)) (length (member (cadr edge) catalog)))) #t)))))
