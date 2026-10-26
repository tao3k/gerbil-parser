#!/usr/bin/env gxi
;;; Observe each Gerbil import and declaration before installing the standard main.
;;; Syntax objects retain this file's locations for PackageSpec and std/make.
;;; Diagnostics use stderr; spec/meta stdout stays machine-readable.
(def (main . arguments)
  (let (importer (gx#current-expander-module-import))
    (def (report prefix value)
      (parameterize ((current-output-port (current-error-port)))
        (displayln prefix value) (force-output)))
    (parameterize
      ((gx#current-expander-module-import
        (lambda (path reload?)
          (report "BUILD-IMPORT " path)
          (let (context (importer path reload?))
            (report "BUILD-IMPORTED " path)
            context))))
      (for-each
        (lambda (form)
          (let (name (gx#stx-e (gx#stx-car form)))
            (report "BUILD-DECLARATION " name)
            (gx#eval-syntax form)
            (report "BUILD-DECLARED " name)))
        (gx#syntax->list
          (quote-syntax
            (
              (import (only-in :std/source this-source-file)
                      (only-in :std/build-script defbuild-script)
                      (only-in :asp-gerbil-scheme/building-api
                               default-exclude-dirs
                               asp-gerbil-scheme-library-package-prototype
                               asp-gerbil-scheme-package-spec!))

              ;; Reference corpora are not Gerbil package sources. clan's native defaults
              ;; already exclude t/, .git/, and .gerbil/.
              (def gerbil-parser-exclude-dirs
                (cons "dev" (cons "tools" (cons "benchmarks" (cons ".data" (cons "target" default-exclude-dirs))))))

              ;; These source files belong to executable and test owners, not the production
              ;; library catalog. Keep the boundary declarative so PackageSpec still performs
              ;; the single source discovery pass.
              (def gerbil-parser-exclude-modules
                '("init.ss"
                  ;; Core Build roles are phase-bound inputs, never runtime roots.
                  "src/compiler/build.ss" "src/ffi/build.ss" "src/grammar/build.ss"
                  "src/language/build.ss" "src/modules/build.ss"
                  "src/runtime/build.ss" "src/testing/build.ss"
                  "build-qualification.ss" "qualify.ss" "build-conformance.ss"
                  "test-processes.ss"
                  "scripts/tests/process-qualification-test.ss"
                  "scripts/tests/history-commit-test.ss"
                  "scripts/generate-source-parser.ss"
                  "scripts/generate-reductions.ss"
                  "scripts/test-plan.ss"
                  "scripts/tests/test-plan-test.ss"
                  "build-test-driver.ss"
                  "build-test-support.ss"
                  "build-language-conformance.ss"
                  "build-language-conformance-link.ss"
                  "build-ffi-tests.ss"
                  "build-rust-ffi-tests.ss"
                  "build-shared-library.ss"
                  "build-gparse.ss"
                  "build-rust-runtime-aot.ss"
                  "generate-rust-runtime.ss"
                  "src/main.ss"
                  "src/cli.ss"
                  "src/ffi/language-abi.ss"
                  "src/ffi/language-abi-context.ss"
                  "src/ffi/rust-aot-abi.ss"
                  "src/ffi/rust-runtime-aot-main.ss"
                  "languages/bash/parser-test.ss"
                  "languages/arithmetic/parser-test.ss"
                  "languages/cypher/parser-test.ss"
                  "languages/gql/parser-test.ss"
                  "languages/hcl/parser-test.ss"
                  "languages/fhirpath/parser-test.ss"
                  "languages/hl7/parser-test.ss"
                  "languages/tla-plus/parser-test.ss"
                  "languages/bash/fixtures.ss"
                  "languages/arithmetic/fixtures.ss"
                  "languages/cypher/fixtures.ss"
                  "languages/gql/fixtures.ss"
                  "languages/hcl/fixtures.ss"
                  "languages/fhirpath/fixtures.ss"
                  "languages/hl7/fixtures.ss"
                  "languages/tla-plus/fixtures.ss"))

              ;; Gambit compiles loadable modules as Mach-O bundles on Darwin.  The
              ;; Homebrew GCC toolchain needs the standard unresolved-symbol policy for an
              ;; FFI bundle; the final AOT consumer resolves these symbols when it links the
              ;; native runtime.
              (def gerbil-parser-ffi-specs
                (let ((include-option
                       (string-append "-I" (path-expand "include"
                                           (path-normalize (path-expand ".." (path-directory (this-source-file)))))))
                      (link-options
                       (cond-expand
                        (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                        (else '()))))
                  (map (lambda (module)
                         `(gxc: ,module "-cc-options" ,include-option ,@link-options))
                       '("src/ffi/language-abi"
                         "src/ffi/language-abi-context"
                         "src/ffi/rust-aot-abi"))))

              ;; PackageSpec remains here because the Build API derives project ownership
              ;; from this declaration's source location. Its default native projection owns
              ;; source discovery; this package entrypoint installs only the reusable library
              ;; and FFI products. The optional gparse command is a sibling AOT product in
              ;; build-gparse.ss, so library consumers never compile an unused executable.
              (asp-gerbil-scheme-package-spec!
               (gerbil-parser-package-spec
               @ asp-gerbil-scheme-library-package-prototype)
               (spec gerbil-parser-build-spec)
               (exclude-dirs gerbil-parser-exclude-dirs)
               (exclude-modules gerbil-parser-exclude-modules)
               (extra-spec gerbil-parser-ffi-specs))

              ;; Keep the standard multicall entrypoint at top level. std/build-script owns
              ;; spec/compile/clean and delegates the projection to one std/make scheduler.
              (defbuild-script (gerbil-parser-build-spec))

            )))))
    (report "BUILD-ENTRY " arguments))
  (apply (eval 'main) arguments))
