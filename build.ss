#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Thin package-build entrypoint; the PackageSpec owns project topology.

(import (only-in :std/source this-source-file)
        (only-in :std/build-script defbuild-script)
        (only-in :asp-gerbil-scheme/building-api
                 default-exclude-dirs
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-spec!))

;; Reference corpora are not Gerbil package sources. clan's native defaults
;; already exclude t/, .git/, and .gerbil/.
(def gerbil-parser-exclude-dirs
  (cons "benchmarks" (cons ".data" (cons "target" default-exclude-dirs))))

;; These source files belong to executable and test owners, not the production
;; library catalog. Keep the boundary declarative so PackageSpec still performs
;; the single source discovery pass.
(def gerbil-parser-exclude-modules
  '("scripts/generate-source-parser.ss"
    "scripts/generate-reductions.ss"
    "scripts/native-preparation-cache.ss"
    "scripts/tests/native-preparation-test.ss"
    "build-native-test-driver.ss"
    "build-language-conformance.ss"
    "build-language-conformance-link.ss"
    "build-native-ffi-tests.ss"
    "build-rust-native-tests.ss"
    "build-native-library.ss"
    "build-gparse.ss"
    "build-rust-runtime-aot.ss"
    "generate-rust-runtime.ss"
    "src/main.ss"
    "src/cli.ss"
    "src/ffi/language-native.ss"
    "src/ffi/parse-artifact-v1-native.ss"
    "src/ffi/rust-runtime-aot-v1-native.ss"
    "src/ffi/rust-runtime-aot-main.ss"
    "languages/bash/parser-test.ss"
    "languages/arithmetic/parser-test.ss"
    "languages/cypher/parser-test.ss"
    "languages/gql/parser-test.ss"
    "languages/hcl/parser-test.ss"
    "languages/fhirpath/parser-test.ss"
    "languages/hl7/parser-test.ss"
    "languages/tla-plus/parser-test.ss"))

;; Gambit compiles loadable modules as Mach-O bundles on Darwin.  The
;; Homebrew GCC toolchain needs the standard unresolved-symbol policy for an
;; FFI bundle; the final AOT consumer resolves these symbols when it links the
;; native runtime.
(def gerbil-parser-native-ffi-specs
  (let ((include-option
         (string-append "-I" (path-expand "include"
                             (path-directory (this-source-file)))))
        (link-options
         (cond-expand
          (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
          (else '()))))
    (map (lambda (module)
           `(gxc: ,module "-cc-options" ,include-option ,@link-options))
         '("src/ffi/language-native"
           "src/ffi/parse-artifact-v1-native"
           "src/ffi/rust-runtime-aot-v1-native"))))

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
 (extra-spec gerbil-parser-native-ffi-specs))

;; Keep the standard multicall entrypoint at top level. std/build-script owns
;; spec/compile/clean and delegates the projection to one std/make scheduler.
(defbuild-script (gerbil-parser-build-spec))
