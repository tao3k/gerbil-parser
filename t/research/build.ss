;;; Standalone research products consume the actual native package import graph.
(import (only-in :std/build-script defbuild-script)
        (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source!
    (path-normalize (path-expand "../.." (path-directory (this-source-file)))) 'gerbil-parser))
(import (only-in "../../src/build-support/compiler-profile.ss" compiler-build-spec initialize-compiler-build!)
        (only-in :std/make pkg-config-libs)
        (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in :clan/poo/object .get)
        (only-in "../fixtures/ffi/build.ss" ffi-build-spec)
        (only-in :yuanxu/module-system/building/source-closure package-library-spec)
        (only-in "../../init.ss" gerbil-parser.init.exports))
(export conformance-build-spec main)

(def package-root
  (path-directory (path-normalize (path-expand "../../gerbil.pkg"
                                                (path-directory (this-source-file))))))
(register-module-build-source! package-root 'gerbil-parser)

 ;; Actual program declarations; std/make owns their output names and builds.
(def research-executable-spec
  (let* ((root package-root)
         (includes (string-append "-I" (path-expand "include" root) " -I" root))
         (libraries (pkg-config-libs "openssl")))
    (map (lambda (entry)
           `(exe: ,(car entry) bin: ,(cadr entry)
                  "-cc-options" ,includes "-ld-options" ,libraries))
    '(("t/research-main" "gerbil-parser-conformance")
      ("t/fixtures/tla-sany-differential/compare" "gerbil-parser-sany-compare")
      ("t/fixtures/tla-sany-differential/corpus" "gerbil-parser-tla-corpus")
      ("t/fixtures/ffi/abi-probe" "gerbil-parser-abi-probe")
      ("t/fixtures/ffi/language-probe" "gerbil-parser-language-probe")
      ("t/benchmarks/capture-prefix/controls" "gerbil-parser-capture-controls")
      ("t/benchmarks/source-edits/benchmark" "gerbil-parser-source-edits")
      ("t/benchmarks/lr-request-stack/benchmark" "gerbil-parser-lr-stack")
      ("t/benchmarks/gql/runtime/matched-stages" "gerbil-parser-gql-profile")
      ("t/benchmarks/gql/runtime/actors" "gerbil-parser-gql-actors")
      ("t/generate-header-delimiters" "gerbil-parser-header-delimiters")
      ("t/generate-module-source" "gerbil-parser-module-source")
      ("t/generate-result-projections" "gerbil-parser-result-projections")
      ("t/generate-ordered-results" "gerbil-parser-ordered-results")
      ("t/generate-command-source" "gerbil-parser-command-source")
      ("t/generate-word-parts" "gerbil-parser-word-parts")
      ("t/generate-region-boundaries" "gerbil-parser-region-boundaries")
      ("t/generate-binding-names" "gerbil-parser-binding-names")
      ("t/generate-source-dependency-witness" "gerbil-parser-source-dependency-witness")
      ("t/fixtures/shared-scanner/generate-bash" "gerbil-parser-bash-scanner")
      ("t/generate-structured-lexical" "gerbil-parser-structured-lexical")
      ("t/generate-quoted-string-profile" "gerbil-parser-quoted-profile")))))
(def research-entry-sources
  (append
    (map (lambda (entry) (string-append (cadr entry) ".ss")) research-executable-spec)
    '("t/benchmarks/poo-grammar-objects/contract.ss"
      "t/benchmarks/versioned-languages/all-languages.ss"
      "t/fixtures/shared-scanner/records-engine-abi.ss"
      ;; The FFI writer owns this runtime root. Core discovers its other imports.
      "t/benchmarks/language-abi/utf8-cpu.ss"
      "t/benchmarks/ranked-literal/parser-cpu.ss"
      "t/benchmarks/ranked-literal/lexical-cpu.ss"
      "t/support/qualification/benchmark-qos.ss")))
(def (research-build-spec)
  ;; Consume the authoritative declarations; do not rebuild their ABI products
  ;; with generic options or maintain a second list of owned identities.
  (def owned
    (append ((.get gerbil-parser.init.exports std-make-spec) '())
            (ffi-build-spec) research-executable-spec))
  (compiler-build-spec
    (map (lambda (source)
           `(gxc: ,source "-cc-options"
                  ,(string-append "-I" (path-expand "include" package-root)
                                  " -I" package-root)))
      (package-library-spec package-root 'gerbil-parser
                            research-entry-sources
                            exclude: (map (lambda (target) (if (string? target) target (cadr target))) owned)))))
(def (conformance-build-spec)
  (append (research-build-spec) (compiler-build-spec research-executable-spec)))

(def package-build-source (path-expand "build.ss" package-root))
(def (run-build spec arguments)
  (def (this-source-file) package-build-source)
  (defbuild-script spec optimize: #t)
  (apply main arguments))

(def (main . arguments)
  (parameterize ((current-output-port (current-error-port)))
    (initialize-compiler-build!))
  (if (or (null? arguments) (equal? (car arguments) "compile"))
    (begin
      (run-build (research-build-spec) arguments)
      (for-each (lambda (target)
                  (run-build (compiler-build-spec (list target)) arguments))
                research-executable-spec))
    (run-build (conformance-build-spec) arguments)))
