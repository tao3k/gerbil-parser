#!/usr/bin/env gxi
;;; Standalone research products consume the actual native package import graph.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/source-closure package-library-spec)
        (only-in "./src/build-support/compiler-profile.ss"
                 compiler-build-spec initialize-compiler-build!))

;; These are program inputs, not a manually maintained transitive helper list.
(def research-entry-sources
  '("t/benchmarks/versioned-languages/all-languages.ss" "t/research-main.ss"
     ;; Actual standalone library input; Core discovers its static closure.
     "t/fixtures/shared-scanner/records-engine-abi.ss"))
(def research-executable-spec
  '((exe: "t/research-main" bin: "gerbil-parser-conformance")))
(def (research-build-spec)
  (compiler-build-spec
    (map (lambda (source)
           `(gxc: ,source "-cc-options"
                  ,(string-append "-I" (path-expand "include" (path-directory (this-source-file))))))
      (package-library-spec (path-directory (this-source-file)) 'gerbil-parser
                            research-entry-sources
                            exclude: (map cadr research-executable-spec)))))
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
(defbuild-script
  (append (research-build-spec)
    (compiler-build-spec research-executable-spec))
  optimize: #t)
