;;; Demand-driven imports must preserve caller bindings and evaluate the loader once.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language))
(def +development-loaded+
  (map (lambda (name) (and (assoc name (list-modules)) #t))
       '("gerbil-parser/src/testing/language-assertions"
         "gerbil-parser/language-support/development"
         "gerbil-parser/src/testing/language-strategy"
         "gerbil-parser/src/testing/language-product")))
(def checked-test-loader 'caller-loader)
(def language-loader-test-clause 'caller-clause)
(def loader-evaluations 0)
(deflanguage-parser-tests test-macro-loader-test "language macro phase ownership"
  (loader (begin (set! loader-evaluations (+ loader-evaluations 1)) arithmetic-language))
  (accepted "language capability still executes parsing" "1 + 2")
  (rejected "language capability still rejects malformed input" "1 +")
  (property "generated imports preserve caller scope and single evaluation"
    (bindings)
    (equal checked-test-loader 'caller-loader)
    (equal language-loader-test-clause 'caller-clause)
    (equal loader-evaluations 1)
    (equal +development-loaded+ '(#f #f #f #f))))
