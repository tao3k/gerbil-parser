;;; Versioned FFI contracts. File paths and namespaces carry no version.
(begin-syntax
  (def contracts
    '((+native-language-abi-version+ 2)
      (+native-codec-abi-version+ 1)
      (+native-event-version+ 1)
      (+native-runtime-aot-abi-version+ 1)
      (+native-descriptor-schema+ "gerbil-parser.native-descriptor.v1")
      (+native-error-schema+ "gerbil-parser.native-error.v1")
      (+native-runtime-aot-error-schema+ "gerbil-parser.rust-runtime-aot-error.v1"))))
(defsyntax (native-schema-value stx)
  (syntax-case stx ()
    ((_ key)
     (datum->syntax #'key (cadr (assq (syntax->datum #'key) contracts))))))
(def +native-language-abi-version+ (native-schema-value +native-language-abi-version+))
(def +native-codec-abi-version+ (native-schema-value +native-codec-abi-version+))
(def +native-event-version+ (native-schema-value +native-event-version+))
(def +native-runtime-aot-abi-version+ (native-schema-value +native-runtime-aot-abi-version+))
(def +native-descriptor-schema+ (native-schema-value +native-descriptor-schema+))
(def +native-error-schema+ (native-schema-value +native-error-schema+))
(def +native-runtime-aot-error-schema+ (native-schema-value +native-runtime-aot-error-schema+))
(export +native-event-version+ +native-language-abi-version+ +native-codec-abi-version+ +native-runtime-aot-abi-version+
        +native-descriptor-schema+ +native-error-schema+ +native-runtime-aot-error-schema+)
;;; Derive the pure C query from the same table; it never enters the Scheme VM.
(defsyntax (declare-language-abi-version stx)
  (datum->syntax #'declare-language-abi-version
    (list 'begin-foreign
      (list 'c-declare
        (string-append "#include <stdint.h>\nuint32_t gerbil_parser_language_abi_version(void) { return "
          (number->string (cadr (assq '+native-language-abi-version+ contracts))) "; }")))))
(export declare-language-abi-version)
