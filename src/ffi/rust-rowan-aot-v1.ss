;;; -*- Gerbil -*-
;;; Build-only C ABI for grammar.ss to immutable Rust/Rowan source.

(import :gerbil/expander
        :std/encoding/json
        (only-in ../compiler/rust-rowan language-rust-rowan-module-source)
        (only-in ../language/module-input language-module-descriptor))
(export native-rowan-aot-abi-version
        native-rust-rowan-source
        native-rowan-aot-error-payload)

(def +native-rowan-aot-abi-version+ 1)
(def +native-rowan-aot-error-schema+
  "gerbil-parser.rust-rowan-aot-error.v1")

(def (native-rowan-aot-abi-version)
  +native-rowan-aot-abi-version+)

(def (native-rowan-aot-error-payload exception)
  (json->string
   (hash (schema +native-rowan-aot-error-schema+)
         (message
          (call-with-output-string
           (lambda (port) (display-exception exception port)))))))

(def (native-rust-rowan-source grammar-path)
  (import-module ':gerbil-parser/rust-rowan-grammar-support #t #t)
  (language-rust-rowan-module-source (language-module-descriptor grammar-path)))
