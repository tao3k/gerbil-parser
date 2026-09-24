#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Developer generator: only structured Rust syntax reaches the output file.

(import (only-in "event-strategy-fixture.ss" parse_event_lines)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 generate-line-event-module))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate-event-strategy-fixture.ss OUTPUT.rs"))

(generate-line-event-module (car (reverse arguments)) parse_event_lines)
