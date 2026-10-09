#!/usr/bin/env gxi
;;; Formal build owner for the native POSIX qualification boundary.
(import (only-in :std/build-script defbuild-script))
(defbuild-script
 `((gxc: "tools/qualification/process-os"
         ,@(cond-expand
            (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
            (else '())))
   (gxc: "tools/qualification/ownership")
   (gxc: "tools/qualification/process")
   (gxc: "tools/qualification/products")
   (gxc: "tools/qualification/environment")
   (gxc: "tools/qualification/performance")
   (gxc: "tools/qualification/plans")
   (exe: "tools/qualification/process-child" bin: "parser-qualification-process-child")
   (exe: "qualify" bin: "parser-qualify")))
