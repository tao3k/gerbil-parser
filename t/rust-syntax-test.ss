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
                 normalized_title normalized_title_rust)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 scheme-pure->rust ascii-ci=?)
        (only-in "rust-aot-test-syntax.ss"
                 check-rust-aot-function check-rust-aot-artifact))
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
       "String" 'trimmed 'to_owned)
      (check (normalized_title "  Alpha  ") => "Alpha")
      (check (normalized_title "\tBeta\n") => "Beta")
      (check-rust-aot-artifact
       normalized_title_rust
       "rust/gerbil-parser-rowan/tests/unit/pure_function_generated.rs"))
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
       true))))
