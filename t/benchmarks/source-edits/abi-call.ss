;;; Real C transport; C-owned input/result storage survives Scheme callbacks.
(import :gerbil-parser/src/ffi/language-abi)
(export parse-source/abi!)
(extern allocate-call invoke-call readback-call release-call)
(def (parse-source/abi! handle source output)
  (unless (and (u8vector? source) (u8vector? output))
    (error "expected Source ABI byte buffers"))
  (let (call (allocate-call source))
    (unless call (error "Source ABI caller allocation failed"))
    (dynamic-wind void
      (lambda ()
        (let (status (invoke-call handle call))
          (if (zero? status) (readback-call call output) status)))
      (lambda () (release-call call)))))
(begin-foreign
 (namespace ("gerbil-parser/t/benchmarks/source-edits/abi-call#"
             allocate-call invoke-call readback-call release-call))
 (c-declare #<<END-C
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <gerbil-parser/language.h>
#define GP_BYTES(obj) ___CAST(uint8_t*, ___BODY_AS((obj), ___tSUBTYPED))
#define GP_SIZE(obj) ___HD_BYTES(___HEADER(obj))
typedef struct source_edit_call {
  uint8_t *source;
  size_t length;
  gerbil_parser_result result;
} source_edit_call;
static source_edit_call *source_edit_allocate(___SCMOBJ source) {
  size_t length = GP_SIZE(source);
  source_edit_call *call = malloc(sizeof(*call));
  if (!call) return NULL;
  call->source = length ? malloc(length) : NULL;
  if (length && !call->source) { free(call); return NULL; }
  if (length) memcpy(call->source, GP_BYTES(source), length);
  call->length = length;
  gerbil_parser_result_init(&call->result);
  return call;
}
static int source_edit_invoke(uint64_t handle, source_edit_call *call) {
  int status = gerbil_parser_language_parse(handle, call->source, call->length, &call->result);
  if (status) fprintf(stderr, "Source transport error: %.*s\n", (int)call->result.length, (const char*)call->result.payload);
  return status;
}
/* A fresh C call receives the relocated Scheme buffer after callback return.
   No Scheme callback or allocation occurs while this buffer is borrowed. */
static int source_edit_readback(source_edit_call *call, ___SCMOBJ output) {
  if (call->result.length != GP_SIZE(output)) {
    fprintf(stderr, "Source payload length: got=%zu expected=%zu\n", call->result.length, (size_t)GP_SIZE(output));
    return -2;
  }
  if (call->result.length) memcpy(GP_BYTES(output), call->result.payload, call->result.length);
  return 0;
}
static void source_edit_release(source_edit_call *call) {
  gerbil_parser_result_release(&call->result);
  free(call->source);
  free(call);
}
END-C
 )
 (c-define-type source-edit-call-ptr (pointer "source_edit_call"))
 (define allocate-call (c-lambda (scheme-object) source-edit-call-ptr "source_edit_allocate"))
 (define invoke-call (c-lambda (unsigned-int64 source-edit-call-ptr) int "source_edit_invoke"))
 (define readback-call (c-lambda (source-edit-call-ptr scheme-object) int "source_edit_readback"))
 (define release-call (c-lambda (source-edit-call-ptr) void "source_edit_release")))
