;;; Normal SDK compiler for the source snapshot materialized by the Just owner.
(import (only-in :gerbil/compiler compile-module))
(def (main source output)
  (compile-module source
    (list output-dir: (path-expand output) optimize: #t generate-ssxi: #t invoke-gsc: #t)))
