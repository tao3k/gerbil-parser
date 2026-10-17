(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Language observation owns its full compiled openCypher grammar fixture.
(import :std/test
        (only-in :gerbil-parser/src/runtime/observability call-with-parser-observed-phase)
        "./scenarios/observability/opencypher-grammar-phases/scenario")
(deflanguage-parser-tests language-grammar-observability-tests "LanguageGrammar Core phase observation"
  (engine)
    (scenario "disabled phase bodies preserve values and evaluate inputs once"
      (scope ((policies 0) (phases 0) (bodies 0))
        (scope-values (((a b)
                      (call-with-parser-observed-phase
                       (begin (set! policies (1+ policies)) #f)
                       (begin (set! phases (1+ phases)) 'contract)
                       (lambda () (set! bodies (1+ bodies)) (values 'a 'b)))))
          (equal (list a b policies phases bodies)  '(a b 1 1 1)))))
    (property "phase calls preserve first-class and computed thunk execution"
      (bindings (calls 0) (invoke call-with-parser-observed-phase) (thunk (lambda () (set! calls (1+ calls)) 'done)))
      (equal (invoke #f 'contract thunk)  'done)
      (equal (call-with-parser-observed-phase #f 'contract thunk)  'done)
      (equal calls  2))
    (property "disabled phase bodies propagate the original exception"
      (bindings)
      (equal (with-catch error-irritants
               (lambda ()
                 (call-with-parser-observed-phase #f 'contract
                  (lambda () (error "phase failure" 'receipt)))))
              '(receipt)))
    (scenario "LanguageGrammar atomically configures POO Flow phase observation"
      (scope ((receipt (opencypher-grammar-observability-scenario)))
        (receipt receipt)
        (equal (opencypher-grammar-observability-scenario-pass? receipt)
                #t))))
(def language-grammar-observability-test language-grammar-observability-tests)
(export  language-grammar-observability-test)
