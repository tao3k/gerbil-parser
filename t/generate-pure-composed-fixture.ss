#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Emit a typed-call fixture from the same executable Scheme definition.

(import (only-in :gerbil-parser/src/compiler/rust-syntax rust-render)
        (only-in "pure-function-fixture.ss" classify_or_unknown_rust))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-pure-composed-fixture.ss OUTPUT.rs"))

(call-with-output-file (car (reverse arguments))
  (lambda (port)
    (write-string (rust-render classify_or_unknown_rust) port)))
