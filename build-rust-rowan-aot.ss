#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional build-time Rowan generator; the runtime package stays independent.

(import (only-in :std/build-script defbuild-script)
        (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-native-pkg-config-options
                 asp-gerbil-scheme-package-spec!))
(export rust-rowan-aot-runtime-modules
        rust-rowan-aot-package
        rust-rowan-aot-build-spec)

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

(asp-gerbil-scheme-package-spec!
 (rust-rowan-aot-package @ asp-gerbil-scheme-library-package-prototype)
 (spec rust-rowan-aot-build-spec)
 (modules
  (append rust-rowan-aot-runtime-modules
          '("src/ffi/rust-rowan-aot-main.ss")))
 (role 'build-support)
 (product-entry-modules '("src/ffi/rust-rowan-aot-main.ss"))
 (native-capabilities '(tls))
 (pkg-config-libs '("openssl"))
 (nix-deps '("openssl"))
 (native-options-resolver
  (lambda ()
    (asp-gerbil-scheme-native-pkg-config-options '("openssl"))))
 (native-spec
  (append
   (map (lambda (module) `(gxc: ,module))
        rust-rowan-aot-runtime-modules)
   '((exe: "src/ffi/rust-rowan-aot-main"
           bin: "gerbil-parser-rowan-aot")))))

(defbuild-script (rust-rowan-aot-build-spec))
