#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Produce the typed hierarchy IR from its executable Scheme algorithm.

(import (only-in "event-fold-fixture.ss" parse_outline_lines))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-outline-fold-ir.ss OUTPUT.json"))
(call-with-output-file (car (reverse arguments))
  (lambda (port) (write-string parse_outline_lines port)))
