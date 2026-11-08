#!/usr/bin/env gxi
;;; Compile the test runtime C exit binding.
(import (only-in "../../../src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in :std/build-script defbuild-script)
        (rename-in (only-in :std/source this-source-file) (this-source-file entry-source-file)))
(export main)

(def (main . arguments)
  (def (this-source-file)
    (path-expand "../../../build.ss" (path-directory (entry-source-file))))
  (defbuild-script
    (compiler-build-spec `((gxc: "t/fixtures/tla-sany-differential/exit-child-process"
            ,@(cond-expand
               (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
               (else '()))))))
  (apply main arguments))
