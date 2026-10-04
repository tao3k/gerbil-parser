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
             ;; ELF keeps separately loaded modules' C symbols local. Declare
             ;; the consumer's actual native link dependencies explicitly.
             (else
              (list "-ld-options"
                    (string-append
                     (path-expand
                      "lib/gerbil-parser/src/ffi/parse-artifact-v1-native~0.o1"
                      (gerbil-path))
                     " "
                     (path-expand
                      "lib/gerbil-parser/src/ffi/rust-rowan-aot-v1-native~0.o1"
                      (gerbil-path)))))))))
