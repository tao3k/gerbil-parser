(import (only-in :std/make pkg-config-libs))
(export gparse-build-spec)

(def (gparse-build-spec)
  `("src/cli"
    (exe: "src/main" bin: "gparse"
          "-ld-options" ,(pkg-config-libs "openssl"))))
