#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Thin package-build entrypoint; the PackageSpec owns project topology.

(import (only-in :std/build-script defbuild-script)
        (only-in :std/srfi/13 string-suffix?)
        (only-in :asp-gerbil-scheme/build-api
                 all-gerbil-modules
                 default-exclude-dirs
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-spec!))

;; Reference corpora are not Gerbil package sources. clan's native defaults
;; already exclude t/, .git/, and .gerbil/.
(def gerbil-parser-exclude-dirs
  (cons ".data" default-exclude-dirs))

;; Discover the production catalog once.  Colocated parser tests remain native
;; gxtest entrypoints; they are not package library modules.
(def gerbil-parser-library-modules
  (filter (lambda (module)
            (and (not (equal? module "src/main.ss"))
                 (not (string-suffix? "-test.ss" module))))
          (all-gerbil-modules exclude-dirs: gerbil-parser-exclude-dirs)))

;; PackageSpec remains here because the Build API derives project ownership
;; from this declaration's source location.  Its default native projection
;; already discovers every non-excluded library module exactly once; extra-spec
;; therefore contains only the executable product.
(asp-gerbil-scheme-package-spec!
 (gerbil-parser-package-spec
 @ asp-gerbil-scheme-library-package-prototype)
 (spec gerbil-parser-build-spec)
 (modules gerbil-parser-library-modules)
 (exclude-dirs gerbil-parser-exclude-dirs)
 (extra-spec '((exe: "src/main" bin: "gparse"))))

;; Keep the standard multicall entrypoint at top level. std/build-script owns
;; spec/compile/clean and delegates the projection to one std/make scheduler.
(defbuild-script (gerbil-parser-build-spec))
