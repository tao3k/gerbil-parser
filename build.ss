#!/usr/bin/env gxi
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :tao3k/yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))

(import (only-in :std/source this-source-file)
        (only-in :std/build-script defbuild-script)
        (only-in :clan/poo/object .get)
        (only-in :tao3k/yuanxu/module-system/building/source-closure package-library-spec)
        (only-in "./init.ss" gerbil-parser.init.exports))

;; Package-level public entrypoints whose import closures are separate from the selected Modules.
(def public-entrypoints
  '("language-build-support.ss"
    "compiler-build-support.ss"
    "event-fold-build-support.ss"
    "event-fold-program-support.ss"
    "rust-runtime-support.ss"
    "line-structure-support.ss"
    "graph-projection-support.ss"
    "artifact-support.ss"
    "source-line-support.ss"
    "parser-actor-support.ss"
    "rust-runtime-event-support.ss"
    "language-test-support.ss"
    "rust-runtime-grammar-support.ss"
    "graph-query-support.ss"
    "graph-syntax-support.ss"
    "src/declarations.ss"
    "language-support.ss"
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
    (def selected (.get gerbil-parser.init.exports std-make-spec))
    (append selected
      (package-library-spec (path-directory (this-source-file)) 'gerbil-parser public-entrypoints
        exclude: (map (lambda (row) (if (string? row) row (cadr row))) selected)))))
(export main)
