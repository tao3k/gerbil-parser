;;; Qualification owns the executable process boundary and its callers.
(import (only-in :std/make pkg-config-libs))
(export qualification-build-spec)

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
