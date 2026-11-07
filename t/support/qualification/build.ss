;;; Qualification owns the executable process boundary and its callers.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in :std/make pkg-config-libs))
(export qualification-build-spec main)

(def (qualification-build-spec)
  `((exe: "t/support/qualification/process-child" bin: "gerbil-parser-process-child"
          "-ld-options" ,(pkg-config-libs "openssl"))
    "t/support/qualification/ownership"
    "t/support/qualification/process"
    "t/support/qualification/compiler-output"
    "t/support/qualification/products"
    "t/support/qualification/environment"
    "t/support/qualification/performance"
    "t/support/qualification/plans"))

(def package-build-source
  (path-expand "../../../build.ss" (path-directory (this-source-file))))

(def (main . arguments)
  (def (this-source-file) package-build-source)
  (defbuild-script (qualification-build-spec) optimize: #t)
  (apply main arguments))
