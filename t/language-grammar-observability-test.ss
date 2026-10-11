;;; -*- Gerbil -*-
;;; Language observation owns its full compiled openCypher grammar fixture.
(import :std/test
        (only-in :gerbil-parser/src/runtime/observability call-with-parser-observed-phase)
        "./scenarios/observability/opencypher-grammar-phases/scenario")
(def language-grammar-observability-tests
  (test-suite "LanguageGrammar Core phase observation"
    (test-case "disabled phase bodies preserve values and evaluate inputs once"
      (let ((policies 0) (phases 0) (bodies 0))
        (let-values (((a b)
                      (call-with-parser-observed-phase
                       (begin (set! policies (1+ policies)) #f)
                       (begin (set! phases (1+ phases)) 'contract)
                       (lambda () (set! bodies (1+ bodies)) (values 'a 'b)))))
          (check (list a b policies phases bodies) => '(a b 1 1 1)))))
    (test-case "phase calls preserve first-class and computed thunk execution"
      (let* ((calls 0) (invoke call-with-parser-observed-phase)
             (thunk (lambda () (set! calls (1+ calls)) 'done)))
        (check (invoke #f 'contract thunk) => 'done)
        (check (call-with-parser-observed-phase #f 'contract thunk) => 'done)
        (check calls => 2)))
    (test-case "disabled phase bodies propagate the original exception"
      (check (with-catch error-irritants
               (lambda ()
                 (call-with-parser-observed-phase #f 'contract
                  (lambda () (error "phase failure" 'receipt)))))
             => '(receipt)))
    (test-case "LanguageGrammar atomically configures POO Flow phase observation"
      (let (receipt (opencypher-grammar-observability-scenario))
        (write receipt) (newline) (force-output)
        (check (opencypher-grammar-observability-scenario-pass? receipt)
               => #t)))))
(def language-grammar-observability-test language-grammar-observability-tests)
(export language-grammar-observability-tests language-grammar-observability-test)
