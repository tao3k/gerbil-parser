#!/usr/bin/env gxi
(import (only-in :std/source this-source-file)
        (only-in :std/build-script defbuild-script)
        (only-in "./build-modules.ss" gerbil-parser-library-modules)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in "./src/build-support/library-publication.ss" publication-build-main))
(def gerbil-parser-ffi-specs
  (let ((include-option
         (string-append "-I" (path-expand "include" (path-directory (this-source-file)))))
        (link-options (cond-expand
                       (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                       (else '()))))
    (map (lambda (module) `(gxc: ,module "-cc-options" ,include-option ,@link-options))
         '("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))))
(defbuild-script (compiler-build-spec (append gerbil-parser-library-modules gerbil-parser-ffi-specs)))
(set! main (publication-build-main main #f))
