#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Produce the Rowan descriptor from the same grammar as the event algorithm.

(import (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in :gerbil-parser/src/compiler/rust-rowan
                 generate-language-rust-rowan-module))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-event-strategy-grammar.ss OUTPUT.rs"))

(generate-language-rust-rowan-module
 (car (reverse arguments)) event-lines-language-grammar)
