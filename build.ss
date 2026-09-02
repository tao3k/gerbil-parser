#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Sole AOT entrypoint for the gerbil-parser library.

(import :std/make
        :clan/building
        (only-in :std/misc/path path-strip-extension)
        (only-in :std/srfi/13 string-prefix? string-suffix?)
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
    "src/compiler/machine"
    "src/compiler/normalize"
    "src/compiler/parser-ir"
    "src/grammar/algebra"
    "src/grammar/lexical-algebra"
    "src/language/entry"
    "src/modules/parser/config"
    "src/modules/parser/funcs"
    "src/modules/parser/objects"
    "src/modules/parser/types"
    "languages/arithmetic/v1/grammar"
    "languages/arithmetic/v1/parser"
    "src/runtime/artifact"
    "src/runtime/combinator"
    "src/runtime/identity"
    "src/runtime/lexer"
    "src/runtime/parser"
    "src/runtime/recognition"
    "src/runtime/scan"
    "src/runtime/token"
    "src/utilities/lists"))

(def +project-modules+
  (map path-strip-extension
       (all-gerbil-modules
        exclude-dirs: (append '("docs") default-exclude-dirs))))

(def (library-modules)
  (filter (lambda (module)
            (and (string-prefix? "src/" module)
                 (not (equal? module "src/main"))
                 (not (member module +gparse-runtime-modules+))))
          +project-modules+))

;; A supported language is an independently owned package surface.  Its Scheme
;; modules are AOT library modules so gxtest can import colocated fixtures in an
;; installed package. Test entry modules and corpora remain source-only, and no
;; only the arithmetic parser used by the CLI enters the exact gparse runtime
;; closure. Other language packs compile after the executable specification,
;; so test fixtures cannot drift the native command closure.
(def (language-package-modules)
  (let* ((modules
          (filter (lambda (module)
                    (and (string-prefix? "languages/" module)
                         (not (string-suffix? "-test" module))
                         (not (member module +gparse-runtime-modules+))))
                  +project-modules+))
         (support
          (filter (cut string-prefix? "languages/support/" <>) modules))
         (grammars
          (filter (cut string-suffix? "/grammar" <>) modules))
         (parsers
          (filter (cut string-suffix? "/parser" <>) modules))
         (remaining
          (filter (lambda (module)
                    (and (not (member module support))
                         (not (member module grammars))
                         (not (member module parsers))))
                  modules)))
    (append support grammars parsers remaining)))

(asp-gerbil-scheme-package-spec!
 (gerbil-parser-package-spec
 @ asp-gerbil-scheme-library-package-prototype)
 (modules +project-modules+)
 (source-catalog-authority 'project)
 (role 'library)
 (profile 'development)
 (roots ["src" "languages" "t"])
 (runtime-roots ["src" "languages/arithmetic/v1"])
 (native-spec
  (append +gparse-runtime-modules+
          '((exe: "src/main" bin: "gparse"))
          (language-package-modules)
          (library-modules))))

(defbuild-script
 (asp-gerbil-scheme-package-native-spec gerbil-parser-package-spec)
 profile: (asp-gerbil-scheme-package-build-profile gerbil-parser-package-spec)
 bindir: (framework-build-bindir))
