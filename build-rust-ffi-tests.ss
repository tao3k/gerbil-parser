#!/usr/bin/env gxi
;;; Link the real Rust static consumer through Gerbil's normal build owner.
(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in "./src/build-support/native-links.ss" native-module-link-options)
        (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(def archive (getenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" #f))
(unless archive (error "GERBIL_PARSER_RUST_NATIVE_ARCHIVE must name the Cargo staticlib"))
(def current-rust-linking? (make-parameter #f))
(defbuild-script
  (compiler-build-spec `((gxc: "t/fixtures/ffi/rust-language-probe"
      "-ld-options" ,(string-append archive
          (cond-expand
           (darwin " -Wl,-undefined,dynamic_lookup")
           (else (if (not (current-rust-linking?)) "" (string-append " -ldl -lpthread -lm "
              (native-module-link-options
                '("gerbil-parser/src/ffi/language-abi~0"
                  "gerbil-parser/t/fixtures/shared-scanner/records-abi~0"))))))))))
  ;; Keep this disposable transport entry out of whole-library optimizer analysis.
  optimize: #f
  force: #t
  verbose: 9)

(let (sdk-main main)
 (set! main (publication-build-main
  (lambda args
    (parameterize ((current-rust-linking?
                    (or (null? args) (member (car args) '("compile" "spec")))))
      (apply sdk-main args))))))
