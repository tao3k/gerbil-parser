#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Repository generation entrypoint for committed Rust/Rowan AOT products.

(import (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/compiler/rust-rowan
                 generate-language-rust-rowan-module))

(def arguments (command-line))
(def output
  (if (> (length arguments) 2)
    (car (reverse arguments))
       "rust/gerbil-parser-rowan-arithmetic/src/generated/mod.rs"))

(generate-language-rust-rowan-module output arithmetic-language-grammar)
