;;; -*- Gerbil -*-
;;; Stable public AOT boundary from one language descriptor to one Rust module.

(import (only-in ./src/compiler/rust-rowan
                 generate-language-rust-rowan-module))
(export generate-language-rust-rowan-module)
