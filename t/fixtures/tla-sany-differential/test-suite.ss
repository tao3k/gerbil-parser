#!/usr/bin/env gxi
;;; Install the existing loader observer before expanding any suite imports.
;;; Source loading expands imports before executing the runtime's first form.
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-compiled-interface-trace
 (lambda () (load "t/fixtures/tla-sany-differential/suite-runtime.ss")))
