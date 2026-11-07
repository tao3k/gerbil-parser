;;; Standalone research products consume the actual native package import graph.
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source!
    (path-normalize (path-expand "../.." (path-directory (this-source-file)))) 'gerbil-parser))
(import (only-in :std/make pkg-config-libs)
        (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in :clan/poo/object .get)
        (only-in "../fixtures/ffi/build.ss" ffi-build-spec)
        (only-in :yuanxu/module-system/building/source-closure package-library-spec)
        (only-in "../../init.ss" gerbil-parser.init.exports))
(export conformance-build-spec)

(def package-root
  (path-directory (path-normalize (path-expand "../../gerbil.pkg"
                                                (path-directory (this-source-file))))))
(register-module-build-source! package-root 'gerbil-parser)

;; These are program inputs, not a manually maintained transitive helper list.
(def research-entry-sources
  '("t/benchmarks/versioned-languages/all-languages.ss" "t/research-main.ss"
     ;; Actual standalone library input; Core discovers its static closure.
     "t/fixtures/shared-scanner/records-engine-abi.ss"
     ;; Official gxi qualification commands load these outside Core's signed
     ;; Suite publication. They need normal library products, not test-lib.
     "t/benchmarks/capture-prefix/controls.ss"
     "t/fixtures/structured-lexical.ss"
     "t/module-source-test.ss"
     "t/fixtures/shared-scanner/bash.ss"
     "t/fixtures/ffi/abi-probe.ss"
     "t/fixtures/ffi/language-probe.ss"
     "t/benchmarks/lr-request-stack/benchmark.ss"))
(def research-executable-spec
  `((exe: "t/research-main" bin: "gerbil-parser-conformance"
          "-cc-options" ,(string-append "-I" (path-expand "include" package-root)
                                       " -I" package-root)
          "-ld-options" ,(pkg-config-libs "openssl"))))
(def (research-build-spec)
  ;; Consume the authoritative declarations; do not rebuild their ABI products
  ;; with generic options or maintain a second list of owned identities.
  (def owned
    (append ((.get gerbil-parser.init.exports std-make-spec) '())
            (ffi-build-spec) research-executable-spec))
  (map (lambda (source)
           `(gxc: ,source "-cc-options"
                  ,(string-append "-I" (path-expand "include" package-root)
                                  " -I" package-root)))
      (package-library-spec package-root 'gerbil-parser
                            research-entry-sources
                            exclude: (map (lambda (target) (if (string? target) target (cadr target))) owned))))
(def (conformance-build-spec)
  (append (research-build-spec) research-executable-spec))
