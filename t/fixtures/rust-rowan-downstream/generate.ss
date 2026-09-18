#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Downstream AOT entry using only the public Rust/Rowan generator facade.

(import (only-in :gerbil-parser/rust-rowan-support
                 generate-language-rust-rowan-module)
        (only-in ./languages/records/v1/grammar records-language-grammar))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate.ss OUTPUT.rs"))

(generate-language-rust-rowan-module
 (car (reverse arguments))
 records-language-grammar)
