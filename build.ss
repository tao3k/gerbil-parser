#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Sole AOT entrypoint for the gerbil-parser library.

(import :std/make
        :clan/building
        (only-in :std/misc/path path-strip-extension)
        (only-in :std/srfi/13 string-prefix?)
        (only-in :asp-gerbil-scheme/src/build-api/package-spec
                 asp-gerbil-scheme-package-spec!
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-build-profile
                 asp-gerbil-scheme-package-native-spec)
        (only-in :asp-gerbil-scheme/src/building/build-script
                 defbuild-script
                 framework-build-bindir))

;; Exact modules reachable from the native command.  Syntax-only and public
;; library facades intentionally stay outside this runtime closure.
(def +gparse-runtime-modules+
  '("src/cli"
    "src/compiler/generate"
    "src/compiler/normalize"
    "src/compiler/parser-ir"
    "src/grammar/algebra"
    "src/modules/parser/config"
    "src/modules/parser/funcs"
    "src/modules/parser/objects"
    "src/reference/arithmetic"
    "src/runtime/lexer"
    "src/runtime/parser"
    "src/runtime/token"
    "src/utilities/lists"))

(def (library-modules)
  (filter (lambda (module)
            (and (string-prefix? "src/" module)
                 (not (equal? module "src/main"))
                 (not (member module +gparse-runtime-modules+))))
          (map path-strip-extension
               (all-gerbil-modules
                exclude-dirs: (append '("t" "docs")
                                      default-exclude-dirs)))))

(asp-gerbil-scheme-package-spec!
 (gerbil-parser-package-spec
  @ asp-gerbil-scheme-library-package-prototype)
 (role 'library)
 (profile 'development)
 (native-spec
  (append +gparse-runtime-modules+
          '((exe: "src/main" bin: "gparse"))
          (library-modules))))

(defbuild-script
 (asp-gerbil-scheme-package-native-spec gerbil-parser-package-spec)
 profile: (asp-gerbil-scheme-package-build-profile gerbil-parser-package-spec)
 bindir: (framework-build-bindir))
