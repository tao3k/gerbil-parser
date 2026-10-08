;;; Real C transport, including result readback and release on every call.
(import :gerbil-parser/src/ffi/language-abi)
(export parse-source/abi!)
(extern parse-source/abi!)
(begin-foreign
 (namespace ("gerbil-parser/t/benchmarks/source-edits/abi-call#" parse-source/abi!))
 (c-declare #<<END-C
#include <string.h>
#include <stdio.h>
#include <gerbil-parser/language.h>
#define GP_BYTES(obj) ___CAST(uint8_t*, ___BODY_AS((obj), ___tSUBTYPED))
#define GP_SIZE(obj) ___HD_BYTES(___HEADER(obj))
static int source_edit_call(uint64_t handle, ___SCMOBJ source, ___SCMOBJ output) {
  gerbil_parser_result result;
  gerbil_parser_result_init(&result);
  int status = gerbil_parser_language_parse(handle, GP_BYTES(source), GP_SIZE(source), &result);
  if (status) { fprintf(stderr, "Source transport error: %.*s\n", (int)result.length, (const char*)result.payload); }
  if (!status && result.length != GP_SIZE(output)) { fprintf(stderr, "Source payload length: got=%zu expected=%zu\n", result.length, (size_t)GP_SIZE(output)); status = -2; }
  if (!status && result.length) memcpy(GP_BYTES(output), result.payload, result.length);
  gerbil_parser_result_release(&result);
  return status;
}
END-C
 )
 (define parse-source/abi!
   (c-lambda (unsigned-int64 scheme-object scheme-object) int "source_edit_call")))
