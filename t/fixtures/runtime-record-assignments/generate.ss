#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Record assignments AOT entry using only the public Rust generator facade.

(import (only-in :gerbil-parser/rust-runtime-support
                 generate-language-rust-runtime-module)
        (only-in "languages/records/grammar.ss"
                 records-language-grammar))

(def arguments (command-line))
(unless (> (length arguments) 2)
  (error "usage: generate.ss OUTPUT.rs"))

(generate-language-rust-runtime-module
 (car (reverse arguments))
 records-language-grammar)
