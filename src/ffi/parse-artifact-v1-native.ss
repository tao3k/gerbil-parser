;;; -*- Gerbil -*-
;;; C ABI projection for the interpreter-safe ParseArtifact v1 value module.

(import (only-in ./parse-artifact-v1
                 native-abi-version
                 native-descriptor-payload
                 native-error-payload
                 native-parse-binary-payload))

(begin-foreign
  (namespace
   ("gerbil-parser/src/ffi/parse-artifact-v1-native#"
    gerbil-parser-native-abi-version
    gerbil-parser-native-descriptor
    gerbil-parser-native-parse
    gerbil-parser-result-v1-set-bytes!
    gerbil_parser_result_v1-status
    gerbil_parser_result_v1-status-set!))

  (c-declare #<<END-C
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define GERBIL_PARSER_U8_DATA(obj) \
  ___CAST(___U8*, ___BODY_AS((obj), ___tSUBTYPED))
#define GERBIL_PARSER_U8_LEN(obj) ___HD_BYTES(___HEADER(obj))

typedef struct {
  int32_t status;
  uint8_t *payload;
  size_t length;
} gerbil_parser_result_v1;

void gerbil_parser_result_v1_init(gerbil_parser_result_v1 *result) {
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}

void gerbil_parser_result_v1_release(gerbil_parser_result_v1 *result) {
  if (result->payload != NULL) {
    free(result->payload);
  }
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}
END-C
  )

  (c-define-type gerbil_parser_result_v1
    (type "gerbil_parser_result_v1" (gerbil_parser_result_v1)))
  (c-define-type gerbil_parser_result_v1-borrowed-ptr*
    (pointer gerbil_parser_result_v1 (gerbil_parser_result_v1*)))

  (define gerbil_parser_result_v1-status
    (c-lambda (gerbil_parser_result_v1-borrowed-ptr*) int32
      "___return(___arg1->status);"))

  (define gerbil_parser_result_v1-status-set!
    (c-lambda (gerbil_parser_result_v1-borrowed-ptr* int32) void
      "___arg1->status = ___arg2; ___return;"))

  (define gerbil-parser-result-v1-set-bytes!
    (c-lambda (gerbil_parser_result_v1-borrowed-ptr* scheme-object) void
    #<<END-C
___arg1->length = GERBIL_PARSER_U8_LEN(___arg2);
___arg1->payload = (uint8_t *)malloc(___arg1->length);
if (___arg1->payload == NULL && ___arg1->length > 0) {
  ___arg1->length = 0;
  ___arg1->status = -1;
  ___return;
}
if (___arg1->length > 0) {
  memcpy(___arg1->payload, GERBIL_PARSER_U8_DATA(___arg2), ___arg1->length);
}
___return;
END-C
    ))

  (c-define (gerbil-parser-native-abi-version)
    () unsigned-int32 "gerbil_parser_native_abi_version" "extern"
    (gerbil-parser/src/ffi/parse-artifact-v1#native-abi-version))

  (c-define (gerbil-parser-native-descriptor language result)
    (UTF-8-string gerbil_parser_result_v1-borrowed-ptr*) int32
    "gerbil_parser_native_descriptor" "extern"
    (with-exception-catcher
     (lambda (exception)
       (gerbil_parser_result_v1-status-set! result -1)
       (gerbil-parser/src/ffi/parse-artifact-v1-native#gerbil-parser-result-v1-set-bytes!
        result
        (string->utf8
         (gerbil-parser/src/ffi/parse-artifact-v1#native-error-payload
          exception)))
       -1)
     (lambda ()
       (gerbil_parser_result_v1-status-set! result 0)
       (gerbil-parser/src/ffi/parse-artifact-v1-native#gerbil-parser-result-v1-set-bytes!
        result
        (string->utf8
         (gerbil-parser/src/ffi/parse-artifact-v1#native-descriptor-payload
          language)))
       (gerbil_parser_result_v1-status result))))

  (c-define (gerbil-parser-native-parse language source result)
    (UTF-8-string UTF-8-string gerbil_parser_result_v1-borrowed-ptr*) int32
    "gerbil_parser_native_parse" "extern"
    (with-exception-catcher
     (lambda (exception)
       (gerbil_parser_result_v1-status-set! result -1)
       (gerbil-parser/src/ffi/parse-artifact-v1-native#gerbil-parser-result-v1-set-bytes!
        result
        (string->utf8
         (gerbil-parser/src/ffi/parse-artifact-v1#native-error-payload
          exception)))
       -1)
     (lambda ()
       (gerbil_parser_result_v1-status-set! result 0)
       (gerbil-parser/src/ffi/parse-artifact-v1-native#gerbil-parser-result-v1-set-bytes!
        result
        (gerbil-parser/src/ffi/parse-artifact-v1#native-parse-binary-payload
         language source))
       (gerbil_parser_result_v1-status result)))))
