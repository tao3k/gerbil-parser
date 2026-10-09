#!/usr/bin/env gxi
;;; Formal build owner for the native POSIX qualification boundary.
(import (only-in :std/build-script defbuild-script))
(defbuild-script
 `((gxc: "t/support/qualification/process-os"
         ,@(cond-expand
            (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
            (else '())))
   (gxc: "t/support/qualification/ownership")
   (gxc: "t/support/qualification/process")
   (gxc: "t/support/qualification/products")
   (gxc: "t/support/qualification/environment")
   (gxc: "t/support/qualification/performance")
   (gxc: "t/support/qualification/plans")))
