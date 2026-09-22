;;; -*- Gerbil -*-
;;; Build-only C ABI for grammar.ss to immutable Rust/Rowan source.

(import :gerbil/expander
        :std/encoding/json
        (only-in ../compiler/machine parser-machine-grammar-digest)
        (only-in ../compiler/rust-rowan rust-rowan-module-source)
        (only-in ../language/descriptor
                 language-grammar?
                 language-grammar-contract
                 language-grammar-ir
                 language-grammar-language
                 language-grammar-machine
                 language-grammar-version))
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

(def (eval-runtime-export exported)
  (let (binding (core-resolve-module-export exported))
    (eval (binding-id binding))))

;;; The precompiled Gerbil expander remains the only grammar syntax owner.
;;; Exactly one exported descriptor prevents accidental multi-language output.
(def (grammar-module-language grammar-path)
  (import-module ':gerbil-parser/rust-rowan-grammar-support #t #t)
  (let* ((context (import-module grammar-path #t #t))
         (languages
          (filter-map
           (lambda (exported)
             (and (= (module-export-phi exported) 0)
                  (let (value (eval-runtime-export exported))
                    (and (language-grammar? value) value))))
           (module-context-export context))))
    (unless (= (length languages) 1)
      (error "grammar module must export exactly one language descriptor"
             grammar-path (length languages)))
    (car languages)))

(def (native-rust-rowan-source grammar-path)
  (let (language (grammar-module-language grammar-path))
    (rust-rowan-module-source
     (language-grammar-language language)
     (language-grammar-version language)
     (language-grammar-contract language)
     (parser-machine-grammar-digest
      (language-grammar-machine language))
     (language-grammar-ir language))))
