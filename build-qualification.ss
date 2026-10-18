#!/usr/bin/env gxi
;;; Formal build owner for the native POSIX qualification boundary.
(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script))
(defbuild-script
 (compiler-build-spec `((gxc: "t/support/qualification/process-child"
         ,@(cond-expand
            (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
            (else '())))
   (gxc: "t/support/qualification/ownership")
   (gxc: "t/support/qualification/process")
   (gxc: "t/support/qualification/compiler-output")
   (gxc: "t/support/qualification/products")
   (gxc: "t/support/qualification/environment")
   (gxc: "t/support/qualification/performance")
   (gxc: "t/support/qualification/plans"))))

(set! main (publication-build-main main))
