;;; -*- Gerbil -*-
;;; Exercise the public C ABI after the normal Gerbil runtime initializes.
(import :gerbil-parser/src/ffi/parse-artifact-v1-native
        :gerbil-parser/src/ffi/rust-runtime-aot-v1-native
        (only-in :gerbil-parser/src/ffi/rust-runtime-aot-v1 native-rust-runtime-source))
(export main)
(extern probe-reuse)

(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/native-ffi/abi-probe#" probe-reuse))
 (c-declare #<<END-C
#include <stdio.h>
#include <string.h>
#include <gerbil-parser/parse-artifact-v1.h>
#include <gerbil-parser/rust-runtime-aot-v1.h>

static int probe_result_reuse(const char *expected) {
  gerbil_parser_result_v1 result;
  int i;
  if (gerbil_parser_native_abi_version() != 1) return 3;
  gerbil_parser_result_v1_init(NULL);
  gerbil_parser_result_v1_release(NULL);
  gerbil_parser_result_v1_init(&result);
  for (i = 0; i < 1000; ++i) {
    if (gerbil_parser_native_parse("gql", "RETURN 1", &result) != 0 ||
        result.status != 0 || result.payload == NULL || result.length < 80) {
      gerbil_parser_result_v1_release(&result);
      return 1;
    }
    if ((i + 1) % 100 == 0) {
      printf("ABI-PARSE-BATCH-OK calls=%d\n", i + 1); fflush(stdout);
    }
  }
  gerbil_parser_result_v1_release(&result);
  if (result.payload != NULL || result.length != 0 || result.status != 0) return 2;
  gerbil_parser_result_v1_release(&result);
  if (gerbil_parser_native_parse("gql", "RETURN 1", NULL) != -1) return 4;
  if (gerbil_parser_native_descriptor("gql", &result) != 0 ||
      result.payload == NULL || result.length == 0) return 5;
  if (gerbil_parser_native_parse("unsupported", "RETURN 1", &result) != -1 ||
      result.status != -1 || result.payload == NULL || result.length == 0) return 6;
  if (gerbil_parser_native_parse("gql", NULL, &result) != -1 ||
      result.status != -1 || result.payload != NULL || result.length != 0) return 7;
  if (gerbil_parser_native_descriptor(NULL, &result) != -1) return 8;
  if (gerbil_parser_native_parse("gql", "RETURN '你好'", &result) != 0 ||
      result.status != 0 || result.payload == NULL) return 9;
  gerbil_parser_result_v1_release(&result);
  puts("ABI-PARSE-ERROR-UTF8-REUSE-OK"); fflush(stdout);

  gerbil_parser_runtime_result_v1 runtime;
  gerbil_parser_runtime_result_v1_init(&runtime);
  gerbil_parser_runtime_result_v1_init(NULL);
  gerbil_parser_runtime_result_v1_release(NULL);
  if (gerbil_parser_runtime_aot_abi_version() != 1 ||
      gerbil_parser_runtime_compile("missing.ss", NULL) != -1) return 10;
  for (i = 0; i < 3; ++i) {
    if (gerbil_parser_runtime_compile(
        "t/fixtures/runtime-record-assignments/languages/records/grammar.ss",
        &runtime) != 0 || runtime.status != 0 || runtime.payload == NULL ||
        runtime.length == 0) return 11;
    if (runtime.length != strlen(expected) ||
        memcmp(runtime.payload, expected, runtime.length) != 0) return 12;
    printf("ABI-RUNTIME-GENERATED-MATCH calls=%d\n", i+1); fflush(stdout);
  }
  if (gerbil_parser_runtime_compile("missing.ss", &runtime) != -1 ||
      runtime.status != -1 || runtime.payload == NULL) return 15;
  if (gerbil_parser_runtime_compile(NULL, &runtime) != -1 ||
      runtime.status != -1 || runtime.payload != NULL || runtime.length != 0) return 16;
  gerbil_parser_runtime_result_v1_release(&runtime);
  gerbil_parser_runtime_result_v1_release(&runtime);
  puts("ABI-RUNTIME-ERROR-REUSE-OK"); fflush(stdout);
  return 0;
}
END-C
 )
 (define probe-reuse (c-lambda (UTF-8-string) int "probe_result_reuse")))

(def (main . _)
  (displayln "NATIVE-ABI-MODULES-LOADED") (force-output)
  (let (expected (native-rust-runtime-source
                 "t/fixtures/runtime-record-assignments/languages/records/grammar.ss"))
    (displayln "NATIVE-ABI-EXPECTED-GENERATED") (force-output)
    (let (status (probe-reuse expected))
      (unless (zero? status) (error "public ABI regression failed" status))))
  (displayln "NATIVE-ABI-OK") (force-output))
