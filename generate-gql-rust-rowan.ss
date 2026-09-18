#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Repository generation entrypoint for the pinned ISO GQL Rust/Rowan product.

(import (only-in :gerbil-parser/languages/gql/iso-39075-2024/grammar
                 gql-iso-language-grammar)
        (only-in :gerbil-parser/src/compiler/rust-rowan
                 generate-language-rust-rowan-module))

(def arguments (command-line))
(def output
  (if (> (length arguments) 2)
    (car (reverse arguments))
       "rust/gerbil-parser-rowan-gql/src/generated/mod.rs"))

(generate-language-rust-rowan-module output gql-iso-language-grammar)
