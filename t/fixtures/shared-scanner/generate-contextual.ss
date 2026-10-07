#!/usr/bin/env gxi
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)
        "records"
        (only-in :gerbil-parser/src/compiler/rust-scanner generate-contextual-language-rust-runtime-module))
(def (main output)
  (generate-contextual-language-rust-runtime-module output records-language-grammar records-contextual-product)
  (displayln "CONTEXTUAL-AOT-GENERATED") (force-output) (test-child-process-exit! 0))
