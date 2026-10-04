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

#include <gerbil-parser/parse-artifact-v1.h>

void gerbil_parser_result_v1_init(gerbil_parser_result_v1 *result) {
  if (result == NULL) return;
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}

void gerbil_parser_result_v1_release(gerbil_parser_result_v1 *result) {
  if (result == NULL) return;
  if (result->payload != NULL) {
    free(result->payload);
  }
  result->status = 0;
  result->payload = NULL;
  result->length = 0;
}

/* Gambit's UTF-8-string callback accepts char*. The public API reads
   immutable strings; conversion does not modify the caller's buffer. */
int32_t gerbil_parser_native_descriptor_impl(char *language, gerbil_parser_result_v1 *result);
int32_t gerbil_parser_native_descriptor(const char *language, gerbil_parser_result_v1 *result) {
  if (result == NULL) return -1;
  if (language == NULL) {
    gerbil_parser_result_v1_release(result);
    result->status = -1;
    return -1;
  }
  return gerbil_parser_native_descriptor_impl((char *)language, result);
}

/* Gambit's UTF-8-string callback accepts char*. The public API reads
   immutable strings; conversion does not modify the caller's buffer. */
int32_t gerbil_parser_native_parse_impl(char *language, char *source, gerbil_parser_result_v1 *result);
int32_t gerbil_parser_native_parse(const char *language, const char *source, gerbil_parser_result_v1 *result) {
  if (result == NULL) return -1;
  if (language == NULL || source == NULL) {
    gerbil_parser_result_v1_release(result);
    result->status = -1;
    return -1;
  }
  return gerbil_parser_native_parse_impl((char *)language, (char *)source, result);
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
size_t length = GERBIL_PARSER_U8_LEN(___arg2);
uint8_t *payload = length == 0 ? NULL : (uint8_t *)malloc(length);
if (payload == NULL && length > 0) {
  gerbil_parser_result_v1_release(___arg1);
  ___arg1->status = -1;
  ___return;
}
if (length > 0) {
  memcpy(payload, GERBIL_PARSER_U8_DATA(___arg2), length);
}
/* Results must be initialized once; every call replaces their owned buffer. */
free(___arg1->payload);
___arg1->payload = payload;
___arg1->length = length;
___return;
END-C
    ))

  (c-define (gerbil-parser-native-abi-version)
    () unsigned-int32 "gerbil_parser_native_abi_version" "extern"
    (gerbil-parser/src/ffi/parse-artifact-v1#native-abi-version))

  (c-define (gerbil-parser-native-descriptor language result)
    (UTF-8-string gerbil_parser_result_v1-borrowed-ptr*) int32
    "gerbil_parser_native_descriptor_impl" "extern"
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
    "gerbil_parser_native_parse_impl" "extern"
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
