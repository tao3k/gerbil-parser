#!/usr/bin/env gxi
;;; Standard entry for the fixture module's ordered producer and consumer builds.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in "./t/fixtures/ffi/build.ss" ffi-build-spec ffi-build-phases))
(export main)

(def (run-ffi-phase spec arguments)
  (defbuild-script (spec))
  (apply main arguments))

(def (main . arguments)
  (cond
   ((or (null? arguments) (equal? (car arguments) "compile"))
    (for-each (lambda (spec)
                (run-ffi-phase (lambda () (compiler-build-spec (spec))) arguments))
              (ffi-build-phases)))
   (else
    (run-ffi-phase (lambda () (ffi-build-spec (equal? (car arguments) "spec")))
                   arguments))))
