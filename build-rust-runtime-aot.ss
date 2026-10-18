#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rust generator; the runtime package stays independent.

(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in :gerbil-parser/src/build-support/rust-runtime-aot
                 rust-runtime-aot-build-spec))

(defbuild-script (compiler-build-spec (rust-runtime-aot-build-spec)))

(set! main (publication-build-main main))
