#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rowan generator; the runtime package stays independent.

(import (only-in :std/build-script defbuild-script))

(def rust-rowan-aot-ffi-spec
  (cond-expand
   (darwin
    '(gxc: "src/ffi/rust-rowan-aot-v1"
           "-ld-options" "-Wl,-undefined,dynamic_lookup"))
   (else
    '(gxc: "src/ffi/rust-rowan-aot-v1"))))

(defbuild-script
  `(,rust-rowan-aot-ffi-spec
    (exe: "src/ffi/rust-rowan-aot-main"
          bin: "gerbil-parser-rowan-aot")))
