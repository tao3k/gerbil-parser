#!/usr/bin/env gxi
;;; Compile the test runtime C exit binding.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(defbuild-script
  `((gxc: "t/fixtures/tla-sany-differential/exit-child-process"
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else '())))))
