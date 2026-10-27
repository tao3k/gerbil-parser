#!/usr/bin/env gxi
;;; Research product preparation and native Suite execution have distinct owners.
(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec initialize-compiler-build!)
        (only-in "./build-test-support.ss" shared-test-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make make make-clean)
        (only-in :std/source this-source-file)
        (only-in :gerbil/compiler compile-module))
(def (compile-research-entry!)
  ;; Source-only product inputs need static Scheme before native executable
  ;; closure discovery. No Suite body belongs to this product compilation.
  (for-each
    (lambda (module)
      (compile-module (string-append module ".ss")
        [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
         optimize: #t generate-ssxi: #t static: #t keep-scm: #t
         invoke-gsc: #f verbose: #f]))
    '("t/benchmarks/versioned-languages/all-languages" "t/research-main")))
(def (compile-static-conformance!)
  ;; Both entries consume the same init-owned support graph; std/make alone
  ;; owns scheduling/currentness. There is no separate research Suite catalog.
  (initialize-compiler-build!)
  (make (shared-test-build-spec)
        srcdir: (path-directory (this-source-file))
        libdir: (path-expand "lib" (gerbil-path))
        optimize: #t)
  ;; Suite modules remain source inputs owned by gerbil test.
  (compile-research-entry!))
;; Developer multicall build retains the standard native shared-module owner.
(defbuild-script (shared-test-build-spec) optimize: #t)
