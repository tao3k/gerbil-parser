;;; -*- Gerbil -*-
;;; Build-only C ABI for grammar.ss to immutable Rust source.

(import (only-in ./schema +native-runtime-aot-abi-version+ +native-runtime-aot-error-schema+)
        :gerbil/expander
        :std/encoding/json
        (only-in ../compiler/rust-runtime language-rust-runtime-module-source)
        (only-in ../language/module-input language-module-descriptor))
(export native-runtime-aot-abi-version
        native-rust-runtime-source
        native-runtime-aot-error-payload)


(def (native-runtime-aot-abi-version)
  +native-runtime-aot-abi-version+)

(def (native-runtime-aot-error-payload exception)
  (json->string
   (hash (schema +native-runtime-aot-error-schema+)
         (message
          (call-with-output-string
           (lambda (port) (display-exception exception port)))))))

(def (native-rust-runtime-source grammar-path)
  (import-module ':gerbil-parser/rust-runtime-grammar-support #t #t)
  (language-rust-runtime-module-source (language-module-descriptor grammar-path)))
