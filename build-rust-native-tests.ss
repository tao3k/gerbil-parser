#!/usr/bin/env gxi
;;; Link the real Rust static consumer through Gerbil's normal build owner.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(def archive (getenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" #f))
(unless archive (error "GERBIL_PARSER_RUST_NATIVE_ARCHIVE must name the Cargo staticlib"))
(defbuild-script
  `((gxc: "t/fixtures/native-ffi/rust-language-probe"
      "-ld-options" ,(string-append archive
          (cond-expand
           (darwin " -Wl,-undefined,dynamic_lookup")
           (else (string-append " -ldl -lpthread -lm "
              (path-expand "lib/gerbil-parser/src/ffi/language-native~0.o1" (gerbil-path)) " "
              (path-expand "lib/gerbil-parser/t/fixtures/shared-scanner/records-native~0.o1" (gerbil-path))))))))
  ;; Keep this disposable transport entry out of whole-library optimizer analysis.
  optimize: #f
  force: #t
  verbose: 9)
