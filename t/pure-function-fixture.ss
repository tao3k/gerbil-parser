;;; -*- Gerbil -*-
;;; AOT function fixture: one Scheme algorithm is executable and compilable.

(import (only-in :std/string/misc string-trim)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 define-rust-pure))
(export normalized_title normalized_title_rust)

(define-rust-pure normalized_title normalized_title_rust
  ((input "&str")) "String"
  (string-trim input))
