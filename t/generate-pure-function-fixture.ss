#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Emit the executable Rust fixture from the same Scheme function tested here.

(import (only-in :gerbil-parser/src/compiler/rust-syntax rust-render)
        (only-in "pure-function-fixture.ss" classify_first_word_rust))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-pure-function-fixture.ss OUTPUT.rs"))

(call-with-output-file (car (reverse arguments))
  (lambda (port)
    (write-string (rust-render classify_first_word_rust) port)))
