#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rowan generator; the runtime package stays independent.

(import (only-in :asp-gerbil-scheme/src/building/build-script
                 defbuild-script
                 framework-build-bindir
                 framework-executable-build-spec))

(def rust-rowan-aot-runtime-modules
  '("src/compiler/funcs"
    "src/compiler/lr"
    "src/runtime/token"
    "src/runtime/recognition"
    "src/runtime/reduce"
    "src/runtime/funcs"
    "src/runtime/observability"
    "src/runtime/lr-parser"
    "src/runtime/scan"
    "src/compiler/machine"
    "src/language/descriptor"
    "src/runtime/identity"
    "src/runtime/language-artifact"
    "src/language/grammar"
    "rust-rowan-grammar-support"
    "src/compiler/rust-rowan"
    "src/ffi/rust-rowan-aot-v1"))

(defbuild-script
 (framework-executable-build-spec
  "src/ffi/rust-rowan-aot-main"
  "gerbil-parser-rowan-aot"
  rust-rowan-aot-runtime-modules
  '()
  '(tls))
 profile: 'production
 bindir: (framework-build-bindir))
