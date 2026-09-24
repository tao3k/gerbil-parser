;;; -*- Gerbil -*-
;;; AOT function fixture: one Scheme algorithm is executable and compilable.

(import (only-in :std/string/misc string-trim)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 define-rust-pure string-first-word string-words
                 string-rest-after-first-word string-in?))
(export normalized_title normalized_title_rust
        classify_first_word classify_first_word_rust
        classify_or_unknown classify_or_unknown_rust
        owned_first_word owned_first_word_rust
        owned_rest_after_first_word owned_rest_after_first_word_rust
        count_words count_words_rust)

(define-rust-pure normalized_title normalized_title_rust
  ((input "&str")) "String"
  (string-trim input))

(define-rust-pure classify_first_word classify_first_word_rust
  ((input "&str") (active "&[&str]") (complete "&[&str]"))
  "&'static str"
  (let* ((candidate (string-first-word input)))
    (if (string-in? candidate active) "active"
      (if (string-in? candidate complete) "complete" ""))))

(define-rust-pure classify_or_unknown classify_or_unknown_rust
  ((input "&str") (active "&[&str]") (complete "&[&str]"))
  "&'static str"
  (using ((classify_first_word "&str" "&[&str]" "&[&str]"))
    (let* ((classification
            (classify_first_word input active complete)))
      (if (equal? classification "") "unknown" classification))))

(define-rust-pure owned_first_word owned_first_word_rust
  ((input "&str") (admitted "bool")) "String"
  (if admitted (string-first-word input) ""))

(define-rust-pure owned_rest_after_first_word
  owned_rest_after_first_word_rust
  ((input "&str")) "String"
  (string-rest-after-first-word input))

(define-rust-pure count_words count_words_rust
  ((input "&str")) "u64"
  (foldl (lambda (_word count) (+ count 1))
         0 (string-words input)))
