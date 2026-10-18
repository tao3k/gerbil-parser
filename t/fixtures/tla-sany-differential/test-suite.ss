#!/usr/bin/env gxi
;;; Install the existing loader observer before expanding any suite imports.
;;; Source loading expands imports before executing the runtime's first form.
;; Completed host GC work is observable during native interface admission.
;; No timer reports progress while a single host operation is blocked.
(gc-report-set! #t)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-module-evaluation-trace
 (lambda () (load "t/fixtures/tla-sany-differential/suite-runtime.ss")))
