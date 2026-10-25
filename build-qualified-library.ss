#!/usr/bin/env -S timeout --signal=TERM --kill-after=5s 180s gxi
;;; Explicit qualification issuance over the ordinary Core/SDK build owner.
(import (rename-in (only-in "./build.ss" main) (main compile-library))
        (only-in "./src/build-support/library-publication.ss" publication-build-main))
(export main)
(def (qualified-command . arguments)
  (def operation (if (null? arguments) "compile" (car arguments)))
  (def active? (and (member operation '("compile" "clean")) #t))
  (def started (##current-time-point))
  (when active?
    (displayln "PACKAGE-BUILD-PHASE parser operation=" operation " stage=entry-command-enter")
    (force-output))
  (begin0 (apply compile-library arguments)
    (when active?
      ;; This follows the actual Core/SDK return, not a backend job-start log.
      (displayln "PACKAGE-BUILD-PHASE parser operation=" operation
                 " stage=entry-command-returned elapsed-s=" (- (##current-time-point) started))
      (force-output))))
(def main (publication-build-main qualified-command #f))
