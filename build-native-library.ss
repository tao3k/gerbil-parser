#!/usr/bin/env gxi
;;; SDK compiler owns static closure and job execution; no generated C rewriting.
(import (only-in :gerbil/compiler compile-module compile-exe execute-pending-compile-jobs!))
(def (main source output cc-options ld-options)
  (displayln "NATIVE-BUNDLE-MODULE-COMPILE") (force-output)
  (def output-directory (path-expand "lib" (path-directory source)))
  (compile-module source
    [output-dir: output-directory invoke-gsc: #f optimize: #f generate-ssxi: #f parallel: #t verbose: #t
     gsc-options: ["-cc-options" cc-options]])
  (execute-pending-compile-jobs!)
  (displayln "NATIVE-BUNDLE-CLOSURE-LINK") (force-output)
  (compile-exe source
    [output-dir: output-directory invoke-gsc: #t output-file: output parallel: #t verbose: #t
     gsc-options: ["-cc-options" cc-options "-ld-options" ld-options]])
  (execute-pending-compile-jobs!))
