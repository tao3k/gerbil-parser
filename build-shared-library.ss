#!/usr/bin/env gxi
(import (only-in "./src/ffi/shared-library/build.ss" build-shared-library!))
(export main)

(def (main source output cc-options ld-options)
  (build-shared-library! source output cc-options ld-options)
  (exit 0))
