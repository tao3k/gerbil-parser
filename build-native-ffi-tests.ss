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
    '("lib/gerbil-parser/src/ffi/language-native~0.o1"
      "lib/gerbil-parser/src/ffi/rust-runtime-aot-native~0.o1"))
   ;; ELF resolves each loadable module's native references independently.
   ;; The generated language entry calls the runtime owner-thread guard.
   (native-spec "t/fixtures/shared-scanner/records-native"
    '("lib/gerbil-parser/src/ffi/language-native~0.o1"))
   (native-spec "t/fixtures/native-ffi/language-probe"
    '("lib/gerbil-parser/src/ffi/language-native~0.o1"
      "lib/gerbil-parser/t/fixtures/shared-scanner/records-native~0.o1"))
   (native-spec "t/benchmarks/source-edits/native-call"
    '("lib/gerbil-parser/src/ffi/language-native~0.o1"))
   (native-spec "t/benchmarks/source-edits/benchmark")))
