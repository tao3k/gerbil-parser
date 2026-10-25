#!/usr/bin/env gxi
;;; Prepare the declared conformance modules with the existing Gerbil build/link owners.
(displayln "CONFORMANCE-PREPARATION-START") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(gc-report-set! #t)
(def (main)
  (call-with-compiled-interface-trace
    (lambda ()
      (load "build-language-conformance.ss")
      (eval '(compile-static-conformance!))
      (load "build-language-conformance-link.ss")
      (eval '(main))))
  (exit 0))
