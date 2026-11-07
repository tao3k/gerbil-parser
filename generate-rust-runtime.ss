#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Repository generation entrypoint for committed Rust AOT products.

(import (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/compiler/rust-runtime
                 generate-language-rust-runtime-module))

(def arguments (command-line))
(def output
  (if (> (length arguments) 2)
    (car (reverse arguments))
       "rust/gerbil-parser-runtime-arithmetic/src/generated/mod.rs"))

(generate-language-rust-runtime-module output arithmetic-language-grammar)
