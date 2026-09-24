#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Write the typed Scheme function IR consumed by gerbil-scheme-rust-ir.

(import (only-in :gerbil-parser/src/compiler/rust-syntax
                 write-rust-function-ir)
        (only-in "pure-function-fixture.ss"
                 owned_rest_after_first_word_rust))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-owned-rest-ir.ss OUTPUT.json"))

(write-rust-function-ir
 (car (reverse arguments)) owned_rest_after_first_word_rust)
