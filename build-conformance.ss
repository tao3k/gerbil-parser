#!/usr/bin/env gxi
;;; Native preparation entry. The content admission owner performs compilation.
(displayln "CONFORMANCE-PREPARATION-START") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(gc-report-set! #t)
(call-with-compiled-interface-trace
 (lambda () (load "scripts/preparation-cache.ss")))
(def (main)
  (prepare-conformance!)
  (exit 0))
