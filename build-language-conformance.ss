#!/usr/bin/env gxi
;;; Standalone research products consume the actual native package import graph.
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make pkg-config-libs)
        (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in :clan/poo/object .get)
        (only-in "./build-ffi-tests.ss" ffi-build-spec)
        (only-in :yuanxu/module-system/building/source-closure package-library-spec)
        (only-in "./src/build-support/compiler-profile.ss"
                 compiler-build-spec initialize-compiler-build!))
(import (only-in "./init.ss" gerbil-parser.init.exports))
(register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser)

 ;; Actual program declarations; std/make owns their output names and builds.
(def research-executable-spec
  (map (lambda (entry)
         `(exe: ,(car entry) bin: ,(cadr entry)
                "-ld-options" ,(pkg-config-libs "openssl")))
    '(("t/research-main" "gerbil-parser-conformance")
      ("t/fixtures/tla-sany-differential/compare" "gerbil-parser-sany-compare")
      ("t/fixtures/tla-sany-differential/corpus" "gerbil-parser-tla-corpus")
      ("t/benchmarks/capture-prefix/controls" "gerbil-parser-capture-controls")
      ("t/benchmarks/source-edits/benchmark" "gerbil-parser-source-edits")
      ("t/benchmarks/lr-request-stack/benchmark" "gerbil-parser-lr-stack")
      ("t/benchmarks/gql/runtime/matched-stages" "gerbil-parser-gql-profile")
      ("t/benchmarks/gql/runtime/actors" "gerbil-parser-gql-actors")
      ("t/generate-header-delimiters" "gerbil-parser-header-delimiters")
      ("t/generate-module-source" "gerbil-parser-module-source")
      ("t/generate-structured-lexical" "gerbil-parser-structured-lexical")
      ("t/generate-quoted-string-profile" "gerbil-parser-quoted-profile"))))
(def research-entry-sources
  (append
    (map (lambda (entry) (string-append (cadr entry) ".ss")) research-executable-spec)
    '("t/benchmarks/versioned-languages/all-languages.ss"
      "t/fixtures/shared-scanner/records-engine-abi.ss"
      ;; The FFI writer owns this runtime root. Core discovers its other imports.
      "t/benchmarks/language-abi/utf8-cpu.ss"
      "t/support/qualification/benchmark-qos.ss")))
(def (research-build-spec)
  ;; Consume the authoritative declarations; do not rebuild their ABI products
  ;; with generic options or maintain a second list of owned identities.
  (def owned
    (append ((.get gerbil-parser.init.exports std-make-spec) '())
            (ffi-build-spec) research-executable-spec))
  (compiler-build-spec
    (map (lambda (source)
           `(gxc: ,source "-cc-options"
                  ,(string-append "-I" (path-expand "include" (path-directory (this-source-file))))))
      (package-library-spec (path-directory (this-source-file)) 'gerbil-parser
                            research-entry-sources
                            exclude: (map (lambda (target) (if (string? target) target (cadr target))) owned)))))
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
 ;; Preserve the native meta/spec/clean protocol. Library and macro objects must
;; finish before executable compilation can regenerate their static interfaces.
(def (research-library-phase)
  (defbuild-script (research-build-spec) optimize: #t)
  main)
(def (research-program-phase)
  (defbuild-script (compiler-build-spec research-executable-spec) optimize: #t)
  main)
(defbuild-script
  (append (research-build-spec) (compiler-build-spec research-executable-spec))
  optimize: #t)
(let (protocol-main main)
  (set! main
    (lambda args
      (if (or (null? args) (equal? (car args) "compile"))
        (begin (apply (research-library-phase) args)
               (apply (research-program-phase) args))
        (apply protocol-main args)))))
