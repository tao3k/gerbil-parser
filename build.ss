#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Thin package-build entrypoint; the PackageSpec owns project topology.

(import (only-in :std/build-script defbuild-script)
        (only-in :clan/poo/object .def .get)
        (only-in :asp-gerbil-scheme/build-api
                 default-exclude-dirs
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-spec!)
        (only-in :poo-flow/src/module-system/observability/build-projection
                 poo-flow-make-observed-package-spec-projector)
        (only-in :poo-flow/src/module-system/observability/config
                 poo-flow-build-observability-policy-prototype))

;; Reference corpora are not Gerbil package sources. clan's native defaults
;; already exclude t/, .git/, and .gerbil/.
(def gerbil-parser-exclude-dirs
  (cons ".data" default-exclude-dirs))

;; These source files belong to executable and test owners, not the production
;; library catalog. Keep the boundary declarative so PackageSpec still performs
;; the single source discovery pass.
(def gerbil-parser-exclude-modules
  '("build-gparse.ss"
    "generate-rust-rowan.ss"
    "generate-gql-rust-rowan.ss"
    "src/main.ss"
    "src/cli.ss"
    "src/ffi/parse-artifact-v1.ss"
    "src/ffi/rust-rowan-aot-v1.ss"
    "languages/arithmetic/v1/parser-test.ss"
    "languages/cypher/opencypher-2024-1/parser-test.ss"
    "languages/gql/iso-39075-2024/parser-test.ss"
    "languages/hcl/v2-24/parser-test.ss"
    "languages/tla-plus/v1/parser-test.ss"))

;; Gambit compiles loadable modules as Mach-O bundles on Darwin.  The
;; Homebrew GCC toolchain needs the standard unresolved-symbol policy for an
;; FFI bundle; the final AOT consumer resolves these symbols when it links the
;; native runtime.
(def gerbil-parser-native-ffi-specs
  (cond-expand
   (darwin
    '((gxc: "src/ffi/parse-artifact-v1"
            "-ld-options" "-Wl,-undefined,dynamic_lookup")
      (gxc: "src/ffi/rust-rowan-aot-v1"
            "-ld-options" "-Wl,-undefined,dynamic_lookup")))
   (else
    '((gxc: "src/ffi/parse-artifact-v1")
      (gxc: "src/ffi/rust-rowan-aot-v1")))))

;; The project derives a named policy from POO Flow's public configuration.
;; Observation decorates PackageSpec projection only; ASP and std/make remain
;; the respective source-catalog and execution owners.
(.def (gerbil-parser-build-observability-policy
       @ poo-flow-build-observability-policy-prototype)
  id: 'build-projection/gerbil-parser
  profile: 'gerbil-parser)

;; PackageSpec remains here because the Build API derives project ownership
;; from this declaration's source location. Its default native projection owns
;; source discovery; this package entrypoint installs only the reusable library
;; and FFI products. The optional gparse command is a sibling AOT product in
;; build-gparse.ss, so library consumers never compile an unused executable.
(asp-gerbil-scheme-package-spec!
 (gerbil-parser-package-spec
 @ asp-gerbil-scheme-library-package-prototype)
 (spec gerbil-parser-build-spec)
 (spec-projector
  (poo-flow-make-observed-package-spec-projector
   (.get asp-gerbil-scheme-library-package-prototype spec-projector)
   gerbil-parser-build-observability-policy))
 (exclude-dirs gerbil-parser-exclude-dirs)
 (exclude-modules gerbil-parser-exclude-modules)
 (extra-spec gerbil-parser-native-ffi-specs))

;; Keep the standard multicall entrypoint at top level. std/build-script owns
;; spec/compile/clean and delegates the projection to one std/make scheduler.
(defbuild-script (gerbil-parser-build-spec))
