#!/usr/bin/env gxi
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))

(import (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/script defbuild-script)
        (only-in :clan/poo/object .get)
        (only-in "./init.ss" gerbil-parser.init.exports)
        (only-in "./build-modules.ss" gerbil-parser-library-modules)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec initialize-compiler-build!))
(def gerbil-parser-ffi-specs
  (let ((include-option
         (string-append "-I" (path-expand "include" (path-directory (this-source-file)))))
        (link-options (cond-expand
                       (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                       (else '()))))
    (map (lambda (module) `(gxc: ,module "-cc-options" ,include-option ,@link-options))
         '("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))))
;; Keep the entire catalog, including FFI options. Core owns source preparation,
;; spec normalization and the exact handoff; the SDK owns compilation/scheduling.
(def modules-spec (.get gerbil-parser.init.exports std-make-spec))
(def retained-spec
  (compiler-build-spec
    (append (map path-strip-extension gerbil-parser-library-modules) gerbil-parser-ffi-specs)))
;; Host compiler policy is process-local; Core/SDK own ordinary package recovery.
(initialize-compiler-build!)
(defbuild-script (modules-spec retained-spec))
