#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Regenerate the exact IR consumed by the generic Rust AOT backend.

(import (only-in "event-fold-fixture.ss" parse_fold_lines))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-event-fold-ir.ss OUTPUT.json"))
(call-with-output-file (car (reverse arguments))
  (lambda (port) (write-string parse_fold_lines port)))
