#!/usr/bin/env gxi
;;; Declare the foreign module without linking a competing dynamic library.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module))
(defbuild-script '((gxc: "../ffi/rust-language-probe") (gxc: "program")))
(let (package-main main)
  (set! main
    (lambda args
      (if (equal? args '("compile"))
        (for-each
          (lambda (source)
            (compile-module (path-expand source (path-directory (this-source-file)))
              [invoke-gsc: #f keep-scm: #t optimize: #t generate-ssxi: #t
               output-dir: (path-expand "lib" (gerbil-path))]))
          ["../ffi/rust-language-probe.ss" "program.ss"])
        (apply package-main args)))))
