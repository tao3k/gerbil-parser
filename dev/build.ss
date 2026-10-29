#!/usr/bin/env gxi
;;; Development policy qualification against the existing native declaration.
(import (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/build-script defbuild-script)
        (only-in :asp-gerbil-scheme/src/build-api/policy-runner
                 asp-gerbil-scheme-run-selected-policy))
(export main)
(def (build-development . args)
  (defbuild-script '("rust-runtime-aot"))
  (apply main args))
(def (check-library-catalog!)
  (parameterize ((current-directory (path-expand ".." (path-directory (this-source-file)))))
    (def (spec script)
      (def output
        (run-process (list "gxi" "-:max-heap=1G,debug=q" script "spec")
          stdout-redirection: #t coprocess: read-all-as-string))
      (call-with-input-string output
        (lambda (input)
          (def value (read input))
          (unless (and (list? value) (eof-object? (read input)))
            (error "native spec command must publish exactly one list datum" script output))
          value)))
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
        (error "selected Module projection differs from native source discovery"
          (filter (lambda (item) (not (member item discovered))) production)
          (filter (lambda (item) (not (member item production))) discovered)))
      ;; Public compatibility and ABI boundaries belong to this native build
      ;; qualification entry, not to a runtime Suite launching a compiler.
      (let ((names (map cadr production)) (seen (make-hash-table)))
        (for-each (lambda (name)
                    (when (hash-key? seen name) (error "duplicate build target" name))
                    (hash-put! seen name #t)) names)
        (for-each (lambda (name)
                    (unless (member name names) (error "missing public build target" name)))
          '("language-support" "language-test-support" "graph-query-support"
            "artifact-support" "source-line-support" "src/runtime/machine"
            "src/runtime/lexical-expression" "src/runtime/lexical-lexer"))
        (for-each (lambda (name)
                    (let (entry (assoc name (map cdr production)))
                      (unless (and entry (member "-cc-options" entry))
                        (error "ABI build target lost native compiler options" name))))
          '("src/ffi/language-abi" "src/ffi/language-abi-context" "src/ffi/rust-aot-abi"))
        (for-each (lambda (name)
                    (when (member name names) (error "retired build target is still published" name)))
          '("src/api" "modules/parser/types" "modules/parser/objects" "modules/parser/config"
            "modules/parser/interface" "src/modules/types" "src/modules/objects"
            "src/modules/loader" "src/modules/parser/interface" "build-modules"
            "src/compiler/machine" "src/compiler/lexical-expression" "src/compiler/lexical-lexer"
            "src/testing/module-loader")))
      (displayln "PARSER-BUILD-CATALOG-OK")
      (map (lambda (target) (string-append (cadr target) ".ss")) production))))
(def (main . args)
  (if (equal? args '("check-catalog"))
    (check-library-catalog!)
    (begin
      (when (or (null? args) (equal? (car args) "compile"))
        (let (selected-sources (check-library-catalog!))
          (parameterize ((current-directory (path-expand ".." (path-directory (this-source-file)))))
            (asp-gerbil-scheme-run-selected-policy selected-sources))))
      (apply build-development args))))
