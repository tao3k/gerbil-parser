#!/usr/bin/env gxi
;;; Generate the Rust witness exclusively from the Scheme grammar declaration.
(import (only-in :gerbil-parser/src/compiler/rust-runtime generate-language-rust-runtime-module)
        (only-in "quoted-string-profile-test.ss" quoted-profile-language-grammar))
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(def (main output)
  (generate-language-rust-runtime-module output quoted-profile-language-grammar)
  (displayln "QUOTED-PROFILE-RUST-GENERATED") (force-output) (test-child-process-exit! 0))
