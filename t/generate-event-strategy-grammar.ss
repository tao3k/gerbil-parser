#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Produce the Rust descriptor from the same grammar as the event algorithm.

(import (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in :gerbil-parser/src/compiler/rust-runtime
                 generate-language-rust-runtime-module))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-event-strategy-grammar.ss OUTPUT.rs"))

(generate-language-rust-runtime-module
 (car (reverse arguments)) event-lines-language-grammar)
