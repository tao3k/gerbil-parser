#!/usr/bin/env gxi
;;; Native Rust consumer program; Cargo owns its required static library.
(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in :std/build-script defbuild-script)
        (only-in :std/make pkg-config-libs)
        (only-in :std/source this-source-file))
(def archive (getenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" #f))
(unless archive (error "GERBIL_PARSER_RUST_NATIVE_ARCHIVE must name the Cargo staticlib"))
(defbuild-script
  (compiler-build-spec
    `((exe: "t/fixtures/ffi/rust-language-probe"
            bin: "gerbil-parser-rust-language-probe"
            "-cc-options" ,(string-append "-I" (path-expand "include" (path-directory (this-source-file))))
            "-ld-options" ,(string-append archive " " (pkg-config-libs "openssl")
              (cond-expand (darwin "") (else " -ldl -lpthread -lm"))))))
  optimize: #f
  force: #t
  verbose: 9)
