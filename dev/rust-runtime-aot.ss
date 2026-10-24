;;; -*- Gerbil -*-
;;; Declarative ASP Building API owner for the optional Rust generator.

(import (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-native-pkg-config-options
                 asp-gerbil-scheme-package-spec!))
(export rust-runtime-aot-runtime-modules
        rust-runtime-aot-package
        rust-runtime-aot-build-spec)

(def rust-runtime-aot-runtime-modules
  '("src/compiler/funcs"
    "src/compiler/lr"
    "src/runtime/token"
    "src/runtime/recognition"
    "src/runtime/reduce"
    "src/runtime/funcs"
    "src/runtime/observability"
    "src/runtime/lr-completion"
    "src/runtime/lr-parser"
    "src/runtime/module-source"
    "src/runtime/lexical-source"
    "src/runtime/scan"
    "src/compiler/machine"
    "src/language/descriptor"
    "src/runtime/identity"
    "src/runtime/language-artifact"
    "src/language/grammar"
    "rust-runtime-grammar-support"
    "src/compiler/rust-runtime"
    "src/language/module-input"
    "src/ffi/rust-runtime-aot"))

(asp-gerbil-scheme-package-spec!
 (rust-runtime-aot-package @ asp-gerbil-scheme-library-package-prototype)
 (spec rust-runtime-aot-build-spec)
 (modules
  (append rust-runtime-aot-runtime-modules
          '("src/ffi/rust-runtime-aot-main.ss")))
 (role 'build-support)
 (product-entry-modules '("src/ffi/rust-runtime-aot-main.ss"))
 (native-capabilities '(tls))
 (pkg-config-libs '("openssl"))
 (nix-deps '("openssl"))
 (native-options-resolver
  (lambda ()
    (asp-gerbil-scheme-native-pkg-config-options '("openssl"))))
 (native-spec
  (append
   (map (lambda (module) `(gxc: ,module))
        rust-runtime-aot-runtime-modules)
   '((exe: "src/ffi/rust-runtime-aot-main"
           bin: "gerbil-parser-runtime-aot")))))
