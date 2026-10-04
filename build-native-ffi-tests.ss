#!/usr/bin/env gxi
;;; Compile the C ABI consumer with the standard module build owner.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(defbuild-script
  `((gxc: "t/fixtures/native-ffi/abi-probe"
          "-cc-options"
          ,(string-append "-I" (path-expand "include"
                               (path-directory (this-source-file))))
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else '())))))
