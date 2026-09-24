#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Compile the Scheme-owned tail of the first word to a Rust test artifact.

(import (only-in :gerbil-parser/src/compiler/rust-syntax
                 write-rust-syntax)
        (only-in "pure-function-fixture.ss"
                 owned_rest_after_first_word_rust))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-owned-rest-fixture.ss OUTPUT.rs"))

(write-rust-syntax
 (car (reverse arguments)) owned_rest_after_first_word_rust)
