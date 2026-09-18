;;; -*- Gerbil -*-
;;; Build-only C ABI for grammar.ss to immutable Rust/Rowan source.

(import :gerbil/expander
        :std/foreign
        :std/text/json
        (only-in :gerbil/gambit call-with-output-string display-exception)
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
  (json-object->string
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

(begin-ffi
  ((struct gerbil_parser_rowan_result_v1 status)
   gerbil-parser-rowan-aot-abi-version
   gerbil-parser-rowan-compile
   gerbil-parser-rowan-result-v1-set-bytes!)

  (c-declare #<<END-C
#include <stdint.h>
#include <stdlib.h>

typedef struct {
  int32_t status;
  uint8_t *payload;
  size_t length;
} gerbil_parser_rowan_result_v1;

void gerbil_parser_rowan_result_v1_init(
    gerbil_parser_rowan_result_v1 *result) {
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}

void gerbil_parser_rowan_result_v1_release(
    gerbil_parser_rowan_result_v1 *result) {
  if (result->payload != NULL) {
    free(result->payload);
  }
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}
END-C
  )

  (define-c-struct gerbil_parser_rowan_result_v1
    ((status . int32))
    #f #f #t)

  (define-c-lambda gerbil-parser-rowan-result-v1-set-bytes!
    (gerbil_parser_rowan_result_v1-borrowed-ptr* scheme-object) void
    #<<END-C
___arg1->length = U8_LEN(___arg2);
___arg1->payload = (uint8_t *)malloc(___arg1->length);
if (___arg1->payload == NULL && ___arg1->length > 0) {
  ___arg1->length = 0;
  ___arg1->status = -1;
  ___return;
}
if (___arg1->length > 0) {
  memcpy(___arg1->payload, U8_DATA(___arg2), ___arg1->length);
}
___return;
END-C
    )

  (c-define (gerbil-parser-rowan-aot-abi-version)
    () unsigned-int32 "gerbil_parser_rowan_aot_abi_version" "extern"
    (gerbil-parser/src/ffi/rust-rowan-aot-v1#native-rowan-aot-abi-version))

  (c-define (gerbil-parser-rowan-compile grammar-path result)
    (UTF-8-string gerbil_parser_rowan_result_v1-borrowed-ptr*) int32
    "gerbil_parser_rowan_compile" "extern"
    (with-exception-catcher
     (lambda (exception)
       (gerbil_parser_rowan_result_v1-status-set! result -1)
       (gerbil-parser/src/ffi/rust-rowan-aot-v1#gerbil-parser-rowan-result-v1-set-bytes!
        result
        (string->utf8
         (gerbil-parser/src/ffi/rust-rowan-aot-v1#native-rowan-aot-error-payload
          exception)))
       -1)
     (lambda ()
       (gerbil_parser_rowan_result_v1-status-set! result 0)
       (gerbil-parser/src/ffi/rust-rowan-aot-v1#gerbil-parser-rowan-result-v1-set-bytes!
        result
        (string->utf8
         (gerbil-parser/src/ffi/rust-rowan-aot-v1#native-rust-rowan-source
          grammar-path)))
       (gerbil_parser_rowan_result_v1-status result)))))
