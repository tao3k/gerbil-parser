;;; -*- Gerbil -*-
;;; The Rust AOT macro constructs syntax values before rendering source.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-struct rust-static rust-array rust-some rust-none
                 rust-number rust-string rust-module
                 rust-identifier rust-method rust-fold rust-binary
                 rust-function-value rust-block
                 rust-function-ir-json
                 rust-function-form-name rust-function-form-parameters
                 rust-struct-form? rust-struct-form-name rust-struct-form-fields
                 rust-field-name rust-module-form-item
                 rust-static-form-value)
        (only-in "pure-function-fixture.ss"
                 normalized_title normalized_title_rust
                 classify_first_word classify_first_word_rust)
        (only-in "pure-function-fixture.ss"
                 classify_or_unknown classify_or_unknown_rust)
        (only-in "pure-function-fixture.ss"
                 owned_first_word owned_first_word_rust
                 owned_rest_after_first_word owned_rest_after_first_word_rust)
        (only-in "pure-function-fixture.ss" count_words count_words_rust)
        (only-in "pure-function-fixture.ss"
                 last_word last_word_rust
                 before_last_word before_last_word_rust
                 boundary_token? boundary_token_rust)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 scheme-pure->rust ascii-ci=? string-words string-after)
        (only-in "rust-aot-test-syntax.ss"
                 check-rust-aot-function check-rust-aot-conditional
                 check-rust-aot-any check-rust-aot-artifact
                 check-rust-function-ir))
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
    (test-case "pure string boundaries retain Scheme and AOT parity"
      (check (last_word "α beta :tag:") => ":tag:")
      (check (before_last_word "α beta :tag:") => "α beta")
      (check (before_last_word "single") => "")
      (check (boundary_token? ":tag:") => #t)
      (check (boundary_token? "title:") => #f)
      (check (rust-function-form-name last_word_rust) => 'last_word)
      (check (rust-function-form-name before_last_word_rust)
             => 'before_last_word)
      (check (rust-function-form-name boundary_token_rust)
             => 'boundary_token_p))
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
       "rust/gerbil-parser-rowan/tests/unit/owned_first_word_generated.rs")
      (check (owned_rest_after_first_word "  WAIT   [#A] Head :tag:  ")
             => "[#A] Head :tag:")
      (check (owned_rest_after_first_word "WAIT") => "")
      (check-rust-function-ir
       owned_rest_after_first_word_rust
       "t/fixtures/owned_rest_after_first_word.ir.json"))
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
    (test-case "Scheme predicate and kebab names normalize at the Rust boundary"
      (let* ((function
              (scheme-pure->rust
               'candidate-valid?
               '((provider-identity . "&str") (complete? . "bool"))
               "bool" 'complete?))
             (ir
              (string->json
               (rust-function-ir-json function)
               (JSONReadOptions object-as-hash: #t)))
             (parameters (hash-get ir "parameters")))
        (check (rust-function-form-name function) => 'candidate_valid_p)
        (check (rust-function-form-parameters function)
               => '((provider_identity . "&str") (complete_p . "bool")))
        (check (hash-get (car parameters) "name")
               => "provider_identity")
        (check (hash-get (cadr parameters) "name")
               => "complete_p")))
    (test-case "nested pure bindings remain typed IR blocks"
      (let* ((function
              (scheme-pure->rust
               'has-prefix
               '((source . "&str") (declarations . "&[String]"))
               "bool"
               '(ormap
                 (lambda (directive)
                   (let* ((prefix (string-before directive "|")))
                     (equal? prefix source)))
                 declarations)))
             (ir (string->json
                  (rust-function-ir-json function)
                  (JSONReadOptions object-as-hash: #t)))
             (body (hash-get (hash-get ir "body") "result")))
        (check (hash-get body "kind") => "any")
        (check (hash-get (hash-get body "body") "kind") => "block")))
    (test-case "typed fold carries Scheme state transition into IR"
      (let* ((function
              (rust-function-value
               'byte_count '((source . "&str")) "u64"
               (rust-block
                '()
                (rust-fold
                 (rust-method (rust-identifier 'source) 'bytes '())
                 'count 'byte (rust-number 0)
                 (rust-binary "+" (rust-identifier 'count)
                              (rust-number 1))))))
             (ir (string->json
                  (rust-function-ir-json function)
                  (JSONReadOptions object-as-hash: #t)))
             (fold (hash-get (hash-get ir "body") "result")))
        (check (hash-get fold "kind") => "fold")
        (check (hash-get fold "accumulator") => "count")
        (check (hash-get fold "item") => "byte")
        (check (hash-get (hash-get fold "step") "operator") => "add")
        (check-exception
         (rust-fold (rust-identifier 'source) 'same 'same
                    (rust-number 0) (rust-number 1))
         true)))
    (test-case "one Scheme fold algorithm executes and lowers to Rust IR"
      (check (count_words "  one\ttwo three  ") => 3)
      (check (count_words "") => 0)
      (let* ((ir (string->json
                  (rust-function-ir-json count_words_rust)
                  (JSONReadOptions object-as-hash: #t)))
             (fold (hash-get (hash-get ir "body") "result")))
        (check (hash-get fold "kind") => "fold")
        (check (hash-get (hash-get fold "iterator") "kind")
               => "words")))
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
