#!/usr/bin/env gxi
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser prelude: 'tao3k/parser/prelude))

(import (only-in :std/source this-source-file)
        (only-in :yuanxu/module-system/building/source-package register-module-build-source!)
        (only-in :yuanxu/module-system/building/script defbuild-script)
        (only-in :clan/poo/object .get)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build!)
        (only-in "./init.ss" gerbil-parser.init.exports))
;; Macro expansion and runtime compiler workers have distinct native load paths.
;; Register the same checked package source in both phases through Core's API.
(register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser prelude: 'tao3k/parser/prelude)
;; The selected Module roles/imports and ABI Build contribution are authoritative.
(def modules-spec (.get gerbil-parser.init.exports std-make-spec))
;; Host compiler policy is process-local; Core and Gerbil own ordinary package recovery.
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
;; Bound retained Gerbil expansion state independently of Gerbil's Gambit/C
;; executor. Respect a smaller configured budget, including serial builds.
;; Import/expansion diagnostics are not part of the standard spec/meta datum.
(defbuild-script
  (parameterize ((current-output-port (current-error-port))) (modules-spec '()))
  parallelize: (min 2 (string->number (getenv "GERBIL_BUILD_CORES" "0"))))
(export main)
