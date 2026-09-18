#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rowan generator; the runtime package stays independent.

(import (only-in :std/build-script defbuild-script)
        (only-in :gerbil-parser/src/build-support/rust-rowan-aot
                 rust-rowan-aot-build-spec))

(defbuild-script (rust-rowan-aot-build-spec))
