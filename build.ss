#!/usr/bin/env gxi
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :tao3k/yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))

(import (only-in :std/source this-source-file)
        (only-in :tao3k/yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in :std/build-script defbuild-script)
        (only-in :tao3k/yuanxu/module-system/building/source-closure package-library-spec)
        (only-in :clan/poo/object .get)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build!)
        (only-in "./init.ss" gerbil-parser.init.exports))
;; Macro expansion and runtime compiler workers have distinct native load paths.
;; Register the same checked package source in both phases through Core's API.
(register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser)
;; The selected Module roles/imports and ABI Build contribution are authoritative.
;; Host compiler policy is process-local; Core and Gerbil own ordinary package recovery.
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
;; Native resource admission owns concurrency for this machine.
;; Preserve explicit caller limits without a package-local worker ceiling.
;; Import/expansion diagnostics are not part of the standard spec/meta datum.
(def package-entry-sources
  '("prelude.ss"
    "language-build-support.ss"
    "compiler-build-support.ss"
    "event-fold-build-support.ss"
    "event-fold-program-support.ss"
    "rust-runtime-support.ss"
    "rust-syntax-support.ss"
    "line-structure-support.ss"
    "graph-projection-support.ss"
    "artifact-support.ss"
    "source-line-support.ss"
    "text-support.ss"
    "parser-actor-support.ss"
    "rust-runtime-event-support.ss"
    "language-test-support.ss"
    "language-support/fixture.ss"
    "rust-runtime-grammar-support.ss"
    "graph-query-support.ss"
    "graph-syntax-support.ss"
    "src/declarations.ss"
    "language-support.ss"
    "language-support/grammar.ss"
    "language-source-support.ss"
    "language-support/iso-bnf-language.ss"
    "language-support/antlr4-language.ss"
    "language-support/javacc-source.ss"
    "languages/arithmetic/parser.ss"
    "languages/bash/parser.ss"
    "languages/cypher/parser.ss"
    "languages/fhirpath/parser.ss"
    "languages/gql/parser.ss"
    "languages/hcl/parser.ss"
    "languages/hl7/parser.ss"
    "languages/tla-plus/parser.ss"
    "language-abi-support.ss"))

(defbuild-script
  (parameterize ((current-output-port (current-error-port)))
    (def module-spec (.get gerbil-parser.init.exports std-make-spec))
    (append module-spec
            (package-library-spec (path-directory (this-source-file)) 'gerbil-parser
              package-entry-sources
              exclude: (map (lambda (target) (if (string? target) target (cadr target))) module-spec)))))
(export main)
