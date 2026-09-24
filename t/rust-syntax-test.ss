;;; -*- Gerbil -*-
;;; The Rust AOT macro constructs syntax values before rendering source.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-struct rust-static rust-array rust-some rust-none
                 rust-number rust-string rust-module
                 rust-struct-form? rust-struct-form-name rust-struct-form-fields
                 rust-field-name rust-module-form-item
                 rust-static-form-value)
        (only-in "pure-function-fixture.ss"
                 normalized_title normalized_title_rust
                 classify_first_word classify_first_word_rust)
        (only-in "pure-function-fixture.ss"
                 classify_or_unknown classify_or_unknown_rust)
        (only-in "pure-function-fixture.ss"
                 owned_first_word owned_first_word_rust)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 scheme-pure->rust ascii-ci=? string-words string-after)
        (only-in "rust-aot-test-syntax.ss"
                 check-rust-aot-function check-rust-aot-conditional
                 check-rust-aot-any check-rust-aot-artifact))
(export rust-syntax-test)

(def rust-syntax-test
  (test-suite "Rust AOT syntax macro"
    (test-case "named fields are syntax nodes, not pre-rendered snippets"
      (let* ((value
              (rust-struct LineStructureSpec
                (grammar_digest (rust-string "sha256:abc"))
                (paragraph_node (rust-some (rust-number 4)))
                (table (rust-none))
                (blocks (rust-array '()))))
             (module (rust-module '("LineStructureSpec")
                                  (rust-static STRUCTURE LineStructureSpec value))))
        (check (rust-struct-form? value) => #t)
        (check (rust-struct-form-name value) => 'LineStructureSpec)
        (check (map rust-field-name (rust-struct-form-fields value))
               => '(grammar_digest paragraph_node table blocks))
        (check (rust-static-form-value (rust-module-form-item module))
               => value)))
    (test-case "pure function body is a structured syntax value"
      (check-rust-aot-function
       normalized_title_rust 'normalized_title '((input . "&str"))
       "String" 'to_owned)
      (check (normalized_title "  Alpha  ") => "Alpha")
      (check (normalized_title "\tBeta\n") => "Beta")
      (check-rust-aot-artifact
       normalized_title_rust
       "rust/gerbil-parser-rowan/tests/unit/pure_function_generated.rs"))
    (test-case "pure conditional and membership stay structural"
      (check-rust-aot-conditional
       classify_first_word_rust 'classify_first_word
       '((input . "&str") (active . "&[&str]")
         (complete . "&[&str]"))
       "&'static str")
      (check (classify_first_word "  WAIT\t Task" '("WAIT") '("DONE"))
             => "active")
      (check (classify_first_word "DONE Task" '("WAIT") '("DONE"))
             => "complete")
      (check (classify_first_word "TODO Task" '("WAIT") '("DONE"))
             => "")
      (check-rust-aot-artifact
       classify_first_word_rust
       "rust/gerbil-parser-rowan/tests/unit/classify_first_word_generated.rs"))
    (test-case "typed pure function composition shares Scheme algorithm"
      (check (classify_or_unknown "WAIT Task" '("WAIT") '("DONE"))
             => "active")
      (check (classify_or_unknown "LATER Task" '("WAIT") '("DONE"))
             => "unknown")
      (check-rust-aot-artifact
       classify_or_unknown_rust
       "rust/gerbil-parser-rowan/tests/unit/classify_or_unknown_generated.rs")
      (check-exception
       (scheme-pure->rust 'invalid '((input . "&str")) "&str"
                          '(classify_first_word input)
                          '((classify_first_word . ("&str" "&[&str]"))))
       true))
    (test-case "owned string results keep the pure Scheme branch semantics"
      (check (owned_first_word "  WAIT Task" #t) => "WAIT")
      (check (owned_first_word "  WAIT Task" #f) => "")
      (check-rust-aot-artifact
       owned_first_word_rust
       "rust/gerbil-parser-rowan/tests/unit/owned_first_word_generated.rs"))
    (test-case "unsupported Scheme effects and free names fail closed"
      (check (ascii-ci=? "seq_todo" "SEQ_TODO") => #t)
      (check (ascii-ci=? "ＴＯＤＯ" "TODO") => #f)
      (check-exception
       (scheme-pure->rust 'invalid '((input . "&str")) "String"
                          '(display input))
       true)
      (check-exception
       (scheme-pure->rust 'invalid '((input . "&str")) "bool"
                          '(string-ci=? input "TODO"))
       true)
      (check-exception
       (scheme-pure->rust 'invalid '((input . "&str")) "String"
                          '(string-before missing "("))
       true))
    (test-case "higher-order word traversal remains an AOT syntax tree"
      (check (string-words "  WAIT(w)\t| DONE(d)  ")
             => '("WAIT(w)" "|" "DONE(d)"))
      (check (string-after "WAIT | DONE" "|") => " DONE")
      (check-rust-aot-any
       (scheme-pure->rust
        'any-declared-name
        '((name . "&str") (declarations . "&[String]")) "bool"
        '(ormap
          (lambda (declaration)
            (ormap (lambda (word)
                     (equal? name (string-before word "(")))
                   (string-words (string-before declaration "|"))))
          declarations))
       'any_declared_name
       '((name . "&str") (declarations . "&[String]")) "bool"))))
