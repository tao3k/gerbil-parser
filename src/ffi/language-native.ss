;;; -*- Gerbil -*-
;;; C ABI for explicit language handles, with no builtin language dependencies.
(import (only-in ../runtime/parse-cost with-parser-cost-stage)
        (only-in ./schema declare-language-abi-version)
        (only-in ./language-handles native-language-handle-descriptor
                 native-language-handle-parse/publish release-native-language!
                 native-language-handle-owner! native-language-handle-count +native-source-byte-limit+)
        (only-in ./language-artifact-codec native-error-payload))
(extern initialize-owner! copy-input! replace-result! result-status)
(def (copy-native-input! input bytes)
  (with-parser-cost-stage 'ffi-input-copy (copy-input! input bytes)))
(def (publish-native-payload! result payload)
  (with-parser-cost-stage 'ffi-result-copy (replace-result! result 0 payload)))
(def (call-with-native-result result thunk)
  (with-exception-catcher
   (lambda (exception)
     (replace-result! result -1 (string->utf8 (native-error-payload exception)))
     -1)
   thunk))
(def (publish-result result thunk)
  (call-with-native-result result
   (lambda ()
     (replace-result! result 0 (thunk))
     (result-status result))))
(begin-foreign
 (namespace ("gerbil-parser/src/ffi/language-native#"
             initialize-owner! copy-input! replace-result! result-status descriptor-callback
             parse-callback release-callback count-callback))
 (c-declare #<<END-C
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdatomic.h>
#include <gerbil-parser/language.h>
#define GP_DATA(obj) ___CAST(___U8*, ___BODY_AS((obj), ___tSUBTYPED))
#define GP_LEN(obj) ___HD_BYTES(___HEADER(obj))
static pthread_t gp_owner;
static _Atomic int gp_owner_ready = 0;
static _Atomic size_t gp_payload_count = 0;
int32_t gerbil_parser_language_is_owner_thread(void) {
  return gp_owner_ready && pthread_equal(gp_owner, pthread_self());
}
void gerbil_parser_result_init(gerbil_parser_result *r) {
  if (r) { r->status = 0; r->payload = NULL; r->length = 0; }
}
void gerbil_parser_result_release(gerbil_parser_result *r) {
  if (r) { if (r->payload) { free(r->payload); atomic_fetch_sub(&gp_payload_count,1); } gerbil_parser_result_init(r); }
}
/* Match Gambit's generated c-define types at this private boundary. On
   LP64 Linux uint64_t may be unsigned long while ___U64 is unsigned long long.
   Public declarations retain their fixed-width C ABI types. */
___S32 gerbil_parser_language_descriptor_impl(___U64, gerbil_parser_result *);
___S32 gerbil_parser_language_parse_impl(___U64, ___U8 *, ___U64, gerbil_parser_result *);
___S32 gerbil_parser_language_release_impl(___U64);
___S32 gerbil_parser_language_handle_count_impl(void);
int32_t gerbil_parser_language_runtime_detach(void) {
  if (!gerbil_parser_language_is_owner_thread()) return -1;
  if (atomic_load(&gp_payload_count)) return -2;
  ___S32 count = gerbil_parser_language_handle_count_impl();
  if (count < 0) return -1;
  if (count) return -2;
  atomic_store(&gp_owner_ready,0);
  return 0;
}
int32_t gerbil_parser_language_descriptor(uint64_t language, gerbil_parser_result *r) {
  if (!r) return -1;
  if (!gerbil_parser_language_is_owner_thread()) { gerbil_parser_result_release(r); r->status=-1; return -1; }
  return gerbil_parser_language_descriptor_impl((___U64)language, r);
}
int32_t gerbil_parser_language_parse(uint64_t language, const uint8_t *source, size_t length, gerbil_parser_result *r) {
  if (!r) return -1;
  if (!gerbil_parser_language_is_owner_thread() || (!source && length) || length > 67108864) {
    gerbil_parser_result_release(r); r->status = -1; return -1;
  }
  return gerbil_parser_language_parse_impl((___U64)language, (___U8 *)source, (___U64)length, r);
}
int32_t gerbil_parser_language_release(uint64_t language) {
  if (!gerbil_parser_language_is_owner_thread()) return -1;
  return gerbil_parser_language_release_impl((___U64)language);
}
END-C
 )
 (c-define-type native-result (type "gerbil_parser_result" (gerbil_parser_result)))
 (c-define-type native-result-ptr (pointer native-result (gerbil_parser_result*)))
 (define initialize-owner! (c-lambda () void "gp_owner=pthread_self();gp_owner_ready=1;___return;"))
 (define result-status (c-lambda (native-result-ptr) int32 "___return(___arg1->status);"))
 (define copy-input! (c-lambda ((pointer unsigned-int8) scheme-object) void
    "if (GP_LEN(___arg2)) memcpy(GP_DATA(___arg2), ___arg1, GP_LEN(___arg2)); ___return;"))
 (define replace-result!
  (c-lambda (native-result-ptr int32 scheme-object) void #<<END-C
size_t n = GP_LEN(___arg3);
uint8_t *p = n ? (uint8_t *)malloc(n) : NULL;
gerbil_parser_result_release(___arg1);
if (n && !p) { ___arg1->status = -1; ___return; }
if (n) { memcpy(p, GP_DATA(___arg3), n); atomic_fetch_add(&gp_payload_count,1); }
___arg1->status = ___arg2; ___arg1->payload = p; ___arg1->length = n;
___return;
END-C
 ))
 (c-define (descriptor-callback language result)
  (unsigned-int64 native-result-ptr) int32 "gerbil_parser_language_descriptor_impl" "extern"
  (gerbil-parser/src/ffi/language-native#publish-result result (lambda () (string->utf8 (gerbil-parser/src/ffi/language-handles#native-language-handle-descriptor language)))))
 (c-define (parse-callback language input length result)
  (unsigned-int64 (pointer unsigned-int8) unsigned-int64 native-result-ptr) int32
  "gerbil_parser_language_parse_impl" "extern"
  (gerbil-parser/src/ffi/language-native#call-with-native-result result
    (lambda ()
      (gerbil-parser/src/ffi/language-handles#native-language-handle-owner!)
      (when (> length gerbil-parser/src/ffi/language-handles#+native-source-byte-limit+) (error "native source byte limit"))
      (let ((bytes (make-u8vector length)))
        (gerbil-parser/src/ffi/language-native#copy-native-input! input bytes)
        (gerbil-parser/src/ffi/language-handles#native-language-handle-parse/publish
         language bytes
         (lambda (payload)
           (gerbil-parser/src/ffi/language-native#publish-native-payload! result payload)
           (zero? (result-status result))))
        (result-status result)))))
 (c-define (count-callback) () int32 "gerbil_parser_language_handle_count_impl" "extern"
  (with-exception-catcher (lambda (_) -1)
    (lambda ()
      ;; Only emptiness matters; avoid narrowing an unbounded Scheme count.
      (if (zero? (gerbil-parser/src/ffi/language-handles#native-language-handle-count)) 0 1))))
 (c-define (release-callback language)
  (unsigned-int64) int32 "gerbil_parser_language_release_impl" "extern"
  (with-exception-catcher (lambda (_) -1)
    (lambda () (gerbil-parser/src/ffi/language-handles#release-native-language! language) 0))))

;; Pin the C callback boundary during module initialization, before publishing
;; any language handle. Foreign pthreads fail before entering the Gambit VM.
(initialize-owner!)

(declare-language-abi-version)
