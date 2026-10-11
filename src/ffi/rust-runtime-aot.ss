;;; -*- Gerbil -*-
;;; Build-only C ABI for grammar.ss to immutable Rust source.

(import (only-in ./schema +rust-aot-abi-version+ +rust-aot-error-schema+)
        :gerbil/expander
        :std/encoding/json
        (only-in ../compiler/rust-runtime language-rust-runtime-module-source)
        (only-in ../language/module-input language-module-descriptor))
(export rust-aot-abi-version
        rust-runtime-source
        rust-aot-error-payload)


(def (rust-aot-abi-version)
  +rust-aot-abi-version+)

(def (rust-aot-error-payload exception)
  (json->string
   (hash (schema +rust-aot-error-schema+)
         (message
          (call-with-output-string
           (lambda (port) (display-exception exception port)))))))

(def (rust-runtime-source grammar-path)
  (import-module ':gerbil-parser/rust-runtime-grammar-support #t #t)
  (language-rust-runtime-module-source (language-module-descriptor grammar-path)))
