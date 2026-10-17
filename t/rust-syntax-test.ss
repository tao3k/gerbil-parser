(import :gerbil-parser/language-test-support)
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
                 rust-function-form-result
                 rust-struct-form? rust-struct-form-name rust-struct-form-fields
                 rust-field-name rust-module-form-item
                 rust-module-form-origin
                 rust-static-form-value)
        (only-in "pure-function-fixture.ss"
                 normalized_title normalized_title_rust
                 classify_first_word classify_first_word_rust)
        (only-in "pure-function-fixture.ss"
                 trim_start_only trim_start_only_rust)
        (only-in "pure-function-fixture.ss"
                 replace_and_append replace_and_append_rust)
        (only-in "pure-function-fixture.ss" slug_words slug_words_rust)
        (only-in "pure-function-fixture.ss"
                 classify_or_unknown classify_or_unknown_rust)
        (only-in "pure-function-fixture.ss"
                 owned_first_word owned_first_word_rust
                 owned_rest_after_first_word owned_rest_after_first_word_rust)
        (only-in "pure-function-fixture.ss" count_words count_words_rust)
        (only-in "pure-function-fixture.ss"
                 map_word_annotations map_word_annotations_rust)
        (only-in "pure-function-fixture.ss"
                 last_word last_word_rust
                 before_last_word before_last_word_rust
                 boundary_token? boundary_token_rust)
        (only-in :gerbil-parser/src/compiler/rust-pure-aot
                 scheme-pure->rust ascii-ci=? string-words string-after
                 string-single-ascii-uppercase? string-unsigned-at-most?)
        (only-in "rust-aot-test-syntax.ss"
                 check-rust-aot-function check-rust-aot-conditional
                 check-rust-aot-any check-rust-aot-artifact
                 check-rust-function-ir))


(deflanguage-parser-tests rust-syntax-test "Rust AOT syntax macro"
  (engine)
    (scenario "named fields are syntax nodes, not pre-rendered snippets"
      (sequential ((value
              (rust-struct LineStructureSpec
                (grammar_digest (rust-string "sha256:abc"))
                (paragraph_node (rust-some (rust-number 4)))
                (table (rust-none))
                (blocks (rust-array '()))))
             (module (rust-module '("LineStructureSpec")
                                  (rust-static STRUCTURE LineStructureSpec value)
                                  "line-structure-runtime.ss")))
        (equal (rust-struct-form? value)  #t)
        (equal (rust-struct-form-name value)  'LineStructureSpec)
        (equal (map rust-field-name (rust-struct-form-fields value))
                '(grammar_digest paragraph_node table blocks))
        (equal (rust-static-form-value (rust-module-form-item module))
                value)
        (equal (rust-module-form-origin module)
                "line-structure-runtime.ss")
        (raises
         (rust-module '("LineStructureSpec") value "invalid\norigin")
         true)))
    (scenario "pure function body is a structured syntax value"
      (verify (check-rust-aot-function
       normalized_title_rust 'normalized_title '((input . "&str"))
       "String" 'to_owned))
      (equal (normalized_title "  Alpha  ")  "Alpha")
      (equal (normalized_title "\tBeta\n")  "Beta")
      (verify (check-rust-aot-artifact
       normalized_title_rust
       "rust/gerbil-parser-runtime/tests/unit/pure_function_generated.rs")))
    (scenario "left trim keeps source trailing whitespace"
      (equal (trim_start_only "  Alpha  ")  "Alpha  ")
      (equal (trim_start_only "\tβ ")  "β ")
      (verify (check-rust-aot-function
       trim_start_only_rust 'trim_start_only '((input . "&str"))
       "String" 'to_owned)))
    (property "pure replacement and append lower as typed string operations"
      (bindings)
      (equal (replace_and_append "a/%s" "b")  "a/bb")
      (equal (replace_and_append "a/" "b")  "a/b")
      (equal (rust-function-form-name replace_and_append_rust)
              'replace_and_append)
      (equal (rust-function-form-result replace_and_append_rust)
              "String"))
    (property "pure lowercase word joins retain Scheme and Rust syntax parity"
      (bindings)
      (equal (slug_words "  Mixed  Case  ")  "mixed-case")
      (equal (slug_words "RÉSUMÉ Notes")  "résumé-notes")
      (equal (rust-function-form-name slug_words_rust)  'slug_words)
      (equal (rust-function-form-result slug_words_rust)  "String"))
    (property "pure string boundaries retain Scheme and AOT parity"
      (bindings)
      (equal (last_word "α beta :tag:")  ":tag:")
      (equal (before_last_word "α beta :tag:")  "α beta")
      (equal (before_last_word "single")  "")
      (equal (boundary_token? ":tag:")  #t)
      (equal (boundary_token? "title:")  #f)
      (equal (rust-function-form-name last_word_rust)  'last_word)
      (equal (rust-function-form-name before_last_word_rust)
              'before_last_word)
      (equal (rust-function-form-name boundary_token_rust)
              'boundary_token_p))
    (scenario "uppercase ASCII and bounded unsigned checks stay typed"
      (equal (string-single-ascii-uppercase? "A")  #t)
      (equal (string-single-ascii-uppercase? "a")  #f)
      (equal (string-single-ascii-uppercase? "É")  #f)
      (equal (string-single-ascii-uppercase? "AB")  #f)
      (equal (string-unsigned-at-most? "064" 64)  #t)
      (equal (string-unsigned-at-most? "65" 64)  #f)
      (equal (string-unsigned-at-most? "+1" 64)  #f)
      (sequential ((single
              (string->json
               (rust-function-ir-json
                (scheme-pure->rust 'single '((value . "&str")) "bool"
                                    '(string-single-ascii-uppercase? value)))
               (JSONReadOptions object-as-hash: #t)))
             (bounded
              (string->json
               (rust-function-ir-json
                (scheme-pure->rust 'bounded '((value . "&str")) "bool"
                                    '(string-unsigned-at-most? value 64)))
               (JSONReadOptions object-as-hash: #t))))
        (equal (hash-ref (hash-ref (hash-ref single "body") "result") "kind")
                "single_ascii_uppercase")
        (equal (hash-ref (hash-ref (hash-ref bounded "body") "result") "kind")
                "unsigned_at_most")))
    (scenario "pure conditional and membership stay structural"
      (verify (check-rust-aot-conditional
       classify_first_word_rust 'classify_first_word
       '((input . "&str") (active . "&[&str]")
         (complete . "&[&str]"))
       "&'static str"))
      (equal (classify_first_word "  WAIT\t Task" '("WAIT") '("DONE"))
              "active")
      (equal (classify_first_word "DONE Task" '("WAIT") '("DONE"))
              "complete")
      (equal (classify_first_word "TODO Task" '("WAIT") '("DONE"))
              "")
      (verify (check-rust-aot-artifact
       classify_first_word_rust
       "rust/gerbil-parser-runtime/tests/unit/classify_first_word_generated.rs")))
    (scenario "typed pure function composition shares Scheme algorithm"
      (equal (classify_or_unknown "WAIT Task" '("WAIT") '("DONE"))
              "active")
      (equal (classify_or_unknown "LATER Task" '("WAIT") '("DONE"))
              "unknown")
      (verify (check-rust-aot-artifact
       classify_or_unknown_rust
       "rust/gerbil-parser-runtime/tests/unit/classify_or_unknown_generated.rs"))
      (raises
       (scheme-pure->rust 'invalid '((input . "&str")) "&str"
                          '(classify_first_word input)
                          '((classify_first_word . ("&str" "&[&str]"))))
       true))
    (scenario "owned string results keep the pure Scheme branch semantics"
      (equal (owned_first_word "  WAIT Task" #t)  "WAIT")
      (equal (owned_first_word "  WAIT Task" #f)  "")
      (verify (check-rust-aot-artifact
       owned_first_word_rust
       "rust/gerbil-parser-runtime/tests/unit/owned_first_word_generated.rs"))
      (equal (owned_rest_after_first_word "  WAIT   [#A] Head :tag:  ")
              "[#A] Head :tag:")
      (equal (owned_rest_after_first_word "WAIT")  "")
      (verify (check-rust-function-ir
       owned_rest_after_first_word_rust
       "t/fixtures/owned_rest_after_first_word.ir.json")))
    (scenario "named pure maps collect typed Rust vectors"
      (equal (map_word_annotations "NEXT(n) WAIT(w@/!)")
              '("NEXT" "WAIT"))
      (equal (rust-function-form-result map_word_annotations_rust)
              "Vec<String>")
      (verify (check-rust-aot-function
       map_word_annotations_rust 'map_word_annotations
       '((input . "&str")) "Vec<String>" 'collect))
      (raises
       (scheme-pure->rust
        'invalid '((input . "&str")) "Vec<String>"
        '(map display (string-words input)))
       true))
    (scenario "unsupported Scheme effects and free names fail closed"
      (equal (ascii-ci=? "seq_todo" "SEQ_TODO")  #t)
      (equal (ascii-ci=? "ＴＯＤＯ" "TODO")  #f)
      (raises
       (scheme-pure->rust 'invalid '((input . "&str")) "String"
                          '(display input))
       true)
      (raises
       (scheme-pure->rust 'invalid '((input . "&str")) "bool"
                          '(string-ci=? input "TODO"))
       true)
      (raises
       (scheme-pure->rust 'invalid '((input . "&str")) "String"
                          '(string-before missing "("))
       true))
    (property "Scheme predicate and kebab names normalize at the Rust boundary"
      (bindings (function
              (scheme-pure->rust
               'candidate-valid?
               '((provider-identity . "&str") (complete? . "bool"))
               "bool" 'complete?)) (ir
              (string->json
               (rust-function-ir-json function)
               (JSONReadOptions object-as-hash: #t))) (parameters (hash-get ir "parameters")))
      (equal (rust-function-form-name function)  'candidate_valid_p)
      (equal (rust-function-form-parameters function)
                '((provider_identity . "&str") (complete_p . "bool")))
      (equal (hash-get (car parameters) "name")
                "provider_identity")
      (equal (hash-get (cadr parameters) "name")
                "complete_p"))
    (property "nested pure bindings remain typed IR blocks"
      (bindings (function
              (scheme-pure->rust
               'has-prefix
               '((source . "&str") (declarations . "&[String]"))
               "bool"
               '(ormap
                 (lambda (directive)
                   (let* ((prefix (string-before directive "|")))
                     (equal? prefix source)))
                 declarations))) (ir (string->json
                  (rust-function-ir-json function)
                  (JSONReadOptions object-as-hash: #t))) (body (hash-get (hash-get ir "body") "result")))
      (equal (hash-get body "kind")  "any")
      (equal (hash-get (hash-get body "body") "kind")  "block"))
    (scenario "typed fold carries Scheme state transition into IR"
      (sequential ((function
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
        (equal (hash-get fold "kind")  "fold")
        (equal (hash-get fold "accumulator")  "count")
        (equal (hash-get fold "item")  "byte")
        (equal (hash-get (hash-get fold "step") "operator")  "add")
        (raises
         (rust-fold (rust-identifier 'source) 'same 'same
                    (rust-number 0) (rust-number 1))
         true)))
    (scenario "one Scheme fold algorithm executes and lowers to Rust IR"
      (equal (count_words "  one\ttwo three  ")  3)
      (equal (count_words "")  0)
      (sequential ((ir (string->json
                  (rust-function-ir-json count_words_rust)
                  (JSONReadOptions object-as-hash: #t)))
             (fold (hash-get (hash-get ir "body") "result")))
        (equal (hash-get fold "kind")  "fold")
        (equal (hash-get (hash-get fold "iterator") "kind")
                "words")))
    (scenario "higher-order word traversal remains an AOT syntax tree"
      (equal (string-words "  WAIT(w)\t| DONE(d)  ")
              '("WAIT(w)" "|" "DONE(d)"))
      (equal (string-after "WAIT | DONE" "|")  " DONE")
      (verify (check-rust-aot-any
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
