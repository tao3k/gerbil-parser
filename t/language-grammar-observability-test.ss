;;; -*- Gerbil -*-
;;; Language observation owns its full compiled openCypher grammar fixture.
(import :std/test
        "./scenarios/observability/opencypher-grammar-phases/scenario")
(def language-grammar-observability-tests
  (test-suite "LanguageGrammar Core phase observation"
    (test-case "LanguageGrammar atomically configures POO Flow phase observation"
      (let (receipt (opencypher-grammar-observability-scenario))
        (write receipt) (newline) (force-output)
        (check (opencypher-grammar-observability-scenario-pass? receipt)
               => #t)))))
(def language-grammar-observability-test language-grammar-observability-tests)
(export language-grammar-observability-tests language-grammar-observability-test)
