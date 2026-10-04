#!/usr/bin/env gxi
;;; Compile the C ABI consumer with the standard module build owner.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(def (native-spec module (objects '()))
  `(gxc: ,module "-cc-options"
          ,(string-append "-I" (path-expand "include" (path-directory (this-source-file)))
                          " -I" (path-directory (this-source-file)))
          ,@(cond-expand
             (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
             (else (if (null? objects) '()
                       (list "-ld-options" (string-join
                         (map (lambda (object) (path-expand object (gerbil-path))) objects) " ")))))))
(defbuild-script
  (list
   (native-spec "t/fixtures/native-ffi/abi-probe"
    '("lib/gerbil-parser/src/ffi/parse-artifact-v1-native~0.o1"
      "lib/gerbil-parser/src/ffi/rust-rowan-aot-v1-native~0.o1"))
   (native-spec "t/fixtures/shared-scanner/records-native")
   (native-spec "t/fixtures/native-ffi/language-v2-probe"
    '("lib/gerbil-parser/src/ffi/language-v2-native~0.o1"
      "lib/gerbil-parser/t/fixtures/shared-scanner/records-native~0.o1"))))
