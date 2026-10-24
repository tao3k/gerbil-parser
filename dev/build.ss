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
(def (check-library-catalog!)
  (parameterize ((current-directory (path-expand ".." (path-directory (this-source-file)))))
    (def (spec script)
      (call-with-input-string
        (run-process (list "gxi" "-:max-heap=1G,debug=q" script "spec")
          stdout-redirection: #t coprocess: read-all-as-string) read))
    ;; Core normalizes native source targets to extensionless identities. Keep
    ;; every compiler option in this independent source-discovery comparison.
    (def (canonical targets)
      (list-sort (lambda (a b) (string<? (cadr a) (cadr b)))
        (map (lambda (target)
               (def name (if (string? target) target (cadr target)))
               (cons (if (string? target) 'gxc: (car target))
                 (cons (if (string-suffix? ".ss" name) (path-strip-extension name) name)
                       (if (string? target) '() (cddr target))))) targets)))
    (let ((production (canonical (spec "build.ss")))
          (discovered (canonical (spec "dev/package-source-build.ss"))))
      (unless (equal? production discovered)
        (error "production catalog differs from native source discovery"
          (filter (lambda (item) (not (member item discovered))) production)
          (filter (lambda (item) (not (member item production))) discovered)))))
  (displayln "PARSER-BUILD-CATALOG-OK"))
(def (main . args)
  (if (equal? args '("check-catalog"))
    (check-library-catalog!)
    (begin
      (when (or (null? args) (equal? (car args) "compile"))
        (check-library-catalog!)
        (parameterize ((current-directory (path-expand ".." (path-directory (this-source-file)))))
          (asp-gerbil-scheme-run-selected-policy
            (append gerbil-parser-library-modules
              '("src/ffi/language-abi.ss" "src/ffi/language-abi-context.ss"
                "src/ffi/rust-aot-abi.ss")))))
      (apply build-development args))))
