prelude: :gerbil/core
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :tao3k/yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))

(import (only-in :std/source this-source-file)
        (only-in :clan/poo/object .get .o)
        (only-in :tao3k/yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in "./src/build-support/build.ss" Build)
        (only-in "./init.ss" gerbil-parser.init.exports))
(export gerbil-parser.package.exports)

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

(def gerbil-parser.package.exports
  (.o (:: @ gerbil-parser.init.exports)
      __std-make-spec:
      (cons '(gxc: "init" (extra-inputs: ("package-spec.ss")))
            (map path-strip-extension public-entrypoints))
      build-actions: Build))
