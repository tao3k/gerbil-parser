;;; FFI declarations derive their values from the shared schema owner.
(import (for-syntax (only-in ../schema +language-abi-version+))
        (only-in ../schema +language-abi-version+ +artifact-abi-version+
                 +event-abi-version+ +rust-aot-abi-version+ +abi-descriptor-schema+
                 +abi-error-schema+ +rust-aot-error-schema+))
(export +event-abi-version+ +language-abi-version+ +artifact-abi-version+ +rust-aot-abi-version+
        +abi-descriptor-schema+ +abi-error-schema+ +rust-aot-error-schema+)
;;; The pure C query uses the shared version value without entering the VM.
(defsyntax (declare-language-abi-version stx)
  (datum->syntax #'declare-language-abi-version
    (list 'begin-foreign
      (list 'c-declare
        (string-append "#include <stdint.h>\nuint32_t gerbil_parser_language_abi_version(void) { return "
          (number->string +language-abi-version+) "; }")))))
(export declare-language-abi-version)
