;;; Versioned FFI contracts. File paths and namespaces carry no version.
(begin-syntax
  (def contracts
    '((+language-abi-version+ 2)
      (+artifact-abi-version+ 1)
      (+event-abi-version+ 1)
      (+rust-aot-abi-version+ 1)
      (+abi-descriptor-schema+ "gerbil-parser.native-descriptor.v1")
      (+abi-error-schema+ "gerbil-parser.native-error.v1")
      (+rust-aot-error-schema+ "gerbil-parser.rust-runtime-aot-error.v1"))))
(defsyntax (abi-schema-value stx)
  (syntax-case stx ()
    ((_ key)
     (datum->syntax #'key (cadr (assq (syntax->datum #'key) contracts))))))
(def +language-abi-version+ (abi-schema-value +language-abi-version+))
(def +artifact-abi-version+ (abi-schema-value +artifact-abi-version+))
(def +event-abi-version+ (abi-schema-value +event-abi-version+))
(def +rust-aot-abi-version+ (abi-schema-value +rust-aot-abi-version+))
(def +abi-descriptor-schema+ (abi-schema-value +abi-descriptor-schema+))
(def +abi-error-schema+ (abi-schema-value +abi-error-schema+))
(def +rust-aot-error-schema+ (abi-schema-value +rust-aot-error-schema+))
(export +event-abi-version+ +language-abi-version+ +artifact-abi-version+ +rust-aot-abi-version+
        +abi-descriptor-schema+ +abi-error-schema+ +rust-aot-error-schema+)
;;; Derive the pure C query from the same table; it never enters the Scheme VM.
(defsyntax (declare-language-abi-version stx)
  (datum->syntax #'declare-language-abi-version
    (list 'begin-foreign
      (list 'c-declare
        (string-append "#include <stdint.h>\nuint32_t gerbil_parser_language_abi_version(void) { return "
          (number->string (cadr (assq '+language-abi-version+ contracts))) "; }")))))
(export declare-language-abi-version)
