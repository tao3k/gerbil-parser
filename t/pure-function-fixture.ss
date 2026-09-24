;;; -*- Gerbil -*-
;;; AOT function fixture: one Scheme algorithm is executable and compilable.

(import (only-in :std/string/misc string-trim)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 define-rust-pure string-first-word string-in?))
(export normalized_title normalized_title_rust
        classify_first_word classify_first_word_rust)

(define-rust-pure normalized_title normalized_title_rust
  ((input "&str")) "String"
  (string-trim input))

(define-rust-pure classify_first_word classify_first_word_rust
  ((input "&str") (active "&[&str]") (complete "&[&str]"))
  "&'static str"
  (let* ((candidate (string-first-word input)))
    (if (string-in? candidate active) "active"
      (if (string-in? candidate complete) "complete" ""))))
