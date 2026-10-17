;;; -*- Gerbil -*-
;;; Stable public AOT boundary from one language descriptor to one Rust module.

(import (only-in ./src/compiler/rust-runtime
                 generate-language-rust-runtime-module))
(import (only-in ./src/compiler/rust-scanner generate-command-source-rust-module generate-rust-scanner-module generate-contextual-language-rust-runtime-module))
(export generate-language-rust-runtime-module generate-command-source-rust-module generate-rust-scanner-module generate-contextual-language-rust-runtime-module)
