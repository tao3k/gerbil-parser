#!/usr/bin/env gxi
;;; Compile the disposable native test child's C exit binding.
(import (only-in :std/build-script defbuild-script))
(defbuild-script
  `((gxc: "t/fixtures/tla-sany-differential/exit-child-process"
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else '())))))
