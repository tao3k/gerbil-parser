#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rust generator; the runtime package stays independent.

(import (only-in :std/build-script defbuild-script)
        (only-in :gerbil-parser/src/build-support/rust-runtime-aot
                 rust-runtime-aot-build-spec))

(defbuild-script (rust-runtime-aot-build-spec))
