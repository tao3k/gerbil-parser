;;; Each source module must declare its macro requirements independently.
(import :gerbil-parser/language-test-support
        (only-in :gerbil/expander import-module current-expander-context eval-syntax))

(def (build-owner-main? path)
  (let (context (import-module (path-expand path) #f #t))
    (parameterize ((current-expander-context context))
      (procedure? (eval-syntax 'main)))))

(deflanguage-parser-tests build-owner-import-test "independent build owner declarations"
  (engine (exclusive #t))
  (matrix "each owner expands without earlier CLI imports"
    (setup)
    ((owner '("build-gparse.ss" "build-qualification.ss" "build-rust-runtime-aot.ss"
              "build-test-driver.ss" "build-test-support.ss" "build-ffi-tests.ss"
              "build-language-conformance.ss")))
    (bindings)
    (equal (build-owner-main? owner) #t))
  (scenario "Rust link owner declares macros with its explicit archive operand"
    (scope ((previous (getenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" #f)))
      (protect
        (body
          ;; No build main is called: only an opaque linker operand is needed.
          (act (setenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" "import-only.a"))
          (equal (build-owner-main? "build-rust-ffi-tests.ss") #t))
        (cleanup
          (branch previous
            (act (setenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" previous))
            (act (setenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE"))))))))
