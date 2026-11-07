;;; Each source module must declare its macro requirements independently.
(import :gerbil-parser/language-test-support
        (only-in :gerbil/expander import-module current-expander-context eval-syntax))

(def (build-owner-main? path)
  (let (context (import-module (path-expand path) #f #t))
    (parameterize ((current-expander-context context))
      (procedure? (eval-syntax 'main)))))

(def (build-owner-metadata path)
  (let (context (import-module (path-expand path) #f #t))
    (parameterize ((current-expander-context context))
      (call-with-input-string
        (eval-syntax
          '(call-with-output-string
             (lambda (port)
               (parameterize ((current-output-port port)) (main "meta")))))
        read-all))))

(deflanguage-parser-tests build-owner-import-test "independent build owner declarations"
  (engine)
  (matrix "each owner expands without earlier CLI imports"
    (setup)
    ((owner '("build-gparse.ss" "build-qualification.ss" "build-rust-runtime-aot.ss"
              "build-test-driver.ss" "build-test-support.ss" "build-ffi-tests.ss"
              "build-language-conformance.ss" "build-shared-library.ss")))
    (bindings)
    (equal (build-owner-main? owner) #t))
  (property "phased FFI construction retains one official metadata response"
    (bindings)
    (equal (build-owner-metadata "build-ffi-tests.ss") '(("spec" "compile" "clean"))))
(scenario "Rust program declarations use the official package lifecycle"
  (scope ((owner "t/fixtures/rust-language-abi/build.ss"))
    (equal (build-owner-main? owner) #t)
    (equal (build-owner-metadata owner) '(("spec" "compile" "clean"))))))
