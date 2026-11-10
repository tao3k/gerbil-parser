#!/usr/bin/env gxi
;;; Run explicitly through gerbil test so unrelated Suite modules do not own
;;; the initial heap of this independent construction benchmark.
(import :gerbil-parser/language-test-support
        (only-in :asp-gerbil-scheme/src/benchmark/framework
                 benchmark-contract-receipt-pass?)
        (only-in "./construction.ss" run-grammar-construction-benchmark))

(deflanguage-parser-tests grammar-construction-benchmark-tests "Checked Grammar construction"
  (engine)
    (scenario "checked object construction satisfies the ASP benchmark gate"
      (scope ((receipt (run-grammar-construction-benchmark)))
        (receipt receipt)
        (equal (benchmark-contract-receipt-pass? receipt) #t))))

(def grammar-construction-benchmark-test grammar-construction-benchmark-tests)
(export grammar-construction-benchmark-test)
