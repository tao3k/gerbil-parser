#!/usr/bin/env gxi
;;; Development policy qualification against the existing native declaration.
(import (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/build-script defbuild-script)
        (only-in "../build-modules.ss" gerbil-parser-library-modules)
        (only-in :asp-gerbil-scheme/src/build-api/policy-runner
                 asp-gerbil-scheme-run-selected-policy))
(export main)
(def (build-development . args)
  (defbuild-script '("rust-runtime-aot"))
  (apply main args))
(def (main . args)
  (when (or (null? args) (equal? (car args) "compile"))
    (parameterize ((current-directory (path-expand ".." (path-directory (this-source-file)))))
      (def (spec script)
        (call-with-input-string
         (run-process (list "gxi" script "spec")
                      stdout-redirection: #t coprocess: read-all-as-string) read))
      ;; Filesystem enumeration order varies across the supported hosts. Compare
      ;; complete target values, including options, after ordering by identity.
      ;; Compilation continues to use the production declaration's own order.
      (def (canonical targets)
        (list-sort (lambda (a b) (string<? (cadr a) (cadr b))) targets))
      (let ((production (spec "build.ss"))
            (discovered (spec "dev/package-source-build.ss")))
        (unless (equal? (canonical production) (canonical discovered))
          (error "production catalog differs from native source discovery"
                 (filter (lambda (item) (not (member item discovered))) production)
                 (filter (lambda (item) (not (member item production))) discovered))))
      (asp-gerbil-scheme-run-selected-policy
       (append gerbil-parser-library-modules
               '("src/ffi/language-abi.ss" "src/ffi/language-abi-context.ss"
                 "src/ffi/rust-aot-abi.ss")))))
  (apply build-development args))
