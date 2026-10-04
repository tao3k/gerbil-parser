#!/usr/bin/env gxi
;;; Generate the Rust witness exclusively from the Scheme grammar declaration.
(import (only-in :gerbil-parser/src/compiler/rust-rowan generate-language-rust-rowan-module)
        (only-in "quoted-string-profile-test.ss" quoted-profile-language-grammar))
(def (main output)
  (generate-language-rust-rowan-module output quoted-profile-language-grammar)
  (displayln "QUOTED-PROFILE-RUST-GENERATED"))
