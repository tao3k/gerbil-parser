#!/usr/bin/env gxi
;;; Research product preparation and native Suite execution have distinct owners.
(import (only-in "./build-test-support.ss" shared-test-build-spec))
(import (only-in :yuanxu/module-system/building/script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module))
(def research-entry-sources
  '("t/benchmarks/versioned-languages/all-languages.ss" "t/research-main.ss"))
(def (compile-research-entry!)
  ;; Source-only product inputs need static Scheme before native executable
  ;; closure discovery. No Suite body belongs to this product compilation.
  (for-each
    (lambda (module)
      (compile-module module
        [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
         optimize: #t generate-ssxi: #t static: #t keep-scm: #t
         invoke-gsc: #f verbose: #f]))
    research-entry-sources))
(def (compile-static-conformance!)
  ;; Core projects the Suite imports and these actual product-root imports.
  ;; std/make owns scheduling; no transitive research helper catalog exists.
  (main "compile")
  ;; Suite modules remain source inputs owned by gerbil test.
  (compile-research-entry!))
;; Developer multicall build retains the standard native shared-module owner.
(defbuild-script (shared-test-build-spec research-entry-sources) optimize: #t)
