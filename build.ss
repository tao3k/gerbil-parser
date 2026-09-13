#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Thin package-build entrypoint; the PackageSpec owns project topology.

(import (only-in :std/build-script defbuild-script)
        (only-in :std/srfi/13 string-prefix? string-suffix?)
        (only-in :asp-gerbil-scheme/build-api
                 all-gerbil-modules
                 default-exclude-dirs
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-spec!))

;; Reference corpora are not Gerbil package sources. clan's native defaults
;; already exclude t/, .git/, and .gerbil/.
(def gerbil-parser-exclude-dirs
  (cons ".data" default-exclude-dirs))

(def gerbil-parser-language-modules
  (filter (lambda (module)
            (and (string-prefix? "languages/" module)
                 (not (string-suffix? "-test.ss" module))))
          (all-gerbil-modules exclude-dirs: gerbil-parser-exclude-dirs)))

;; PackageSpec remains here because the Build API derives project ownership
;; from this declaration's source location.
(asp-gerbil-scheme-package-spec!
 (gerbil-parser-package-spec
 @ asp-gerbil-scheme-library-package-prototype)
 (spec gerbil-parser-build-spec)
 (exclude-dirs gerbil-parser-exclude-dirs)
 (exclude-modules '("src/main.ss"))
 (extra-spec
  (append gerbil-parser-language-modules
          '((exe: "src/main" bin: "gparse")))))

;; Keep the standard multicall entrypoint at top level. std/build-script owns
;; spec/compile/clean and delegates the projection to one std/make scheduler.
(defbuild-script (gerbil-parser-build-spec))
