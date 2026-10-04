#!/usr/bin/env gxi
(import "records"
        (only-in :gerbil-parser/src/compiler/rust-scanner generate-contextual-language-rust-rowan-module))
(def (main output)
  (generate-contextual-language-rust-rowan-module output records-language-grammar records-contextual-product)
  (displayln "CONTEXTUAL-AOT-GENERATED"))
