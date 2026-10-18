#!/usr/bin/env gxi
;;; Compile the test runtime C exit binding.
(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(defbuild-script
  (compiler-build-spec `((gxc: "t/fixtures/tla-sany-differential/exit-child-process"
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else '()))))))

(set! main (publication-build-main main))
