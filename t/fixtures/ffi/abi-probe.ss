;;; -*- Gerbil -*-
;;; Exercise the public C ABI after the normal Gerbil runtime initializes.
(import :gerbil-parser/src/ffi/language-abi
        :gerbil-parser/src/ffi/rust-aot-abi
        (only-in :gerbil-parser/src/ffi/rust-runtime-aot rust-runtime-source))
(import (only-in :gerbil-parser/languages/gql/parser gql-language-grammar)
        (only-in :gerbil-parser/src/ffi/language-handles register-language! release-language!))
(export main)
(extern probe-reuse)

(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/ffi/abi-probe#" probe-reuse))
 (c-declare #<<END-C
#include <stdio.h>
#include <string.h>
#include <gerbil-parser/language.h>
#include <gerbil-parser/rust-runtime-aot.h>

static int parse_source(uint64_t language, const char *source, gerbil_parser_result *result) {
  return gerbil_parser_language_parse(language, (const uint8_t *)source, source ? strlen(source) : 1, result);
}
static int probe_result_reuse(uint64_t language, const char *expected) {
  gerbil_parser_result result;
  int i;
  if (gerbil_parser_language_abi_version() != 2) return 3;
  gerbil_parser_result_init(NULL);
  gerbil_parser_result_release(NULL);
  gerbil_parser_result_init(&result);
  for (i = 0; i < 1000; ++i) {
    if (parse_source(language, "RETURN 1", &result) != 0 ||
        result.status != 0 || result.payload == NULL || result.length < 80) {
      gerbil_parser_result_release(&result);
      return 1;
    }
    if ((i + 1) % 100 == 0) {
      printf("ABI-PARSE-BATCH-OK calls=%d\n", i + 1); fflush(stdout);
    }
  }
  gerbil_parser_result_release(&result);
  if (result.payload != NULL || result.length != 0 || result.status != 0) return 2;
  gerbil_parser_result_release(&result);
  if (parse_source(language, "RETURN 1", NULL) != -1) return 4;
  if (gerbil_parser_language_descriptor(language, &result) != 0 ||
      result.payload == NULL || result.length == 0) return 5;
  if (parse_source(0, "RETURN 1", &result) != -1 ||
      result.status != -1 || result.payload == NULL || result.length == 0) return 6;
  if (parse_source(language, NULL, &result) != -1 ||
      result.status != -1 || result.payload != NULL || result.length != 0) return 7;
  if (gerbil_parser_language_descriptor(0, &result) != -1) return 8;
  if (parse_source(language, "RETURN '你好'", &result) != 0 ||
      result.status != 0 || result.payload == NULL) return 9;
  gerbil_parser_result_release(&result);
  puts("ABI-PARSE-ERROR-UTF8-REUSE-OK"); fflush(stdout);

  gerbil_parser_runtime_result runtime;
  gerbil_parser_runtime_result_init(&runtime);
  gerbil_parser_runtime_result_init(NULL);
  gerbil_parser_runtime_result_release(NULL);
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
  gerbil_parser_runtime_result_release(&runtime);
  gerbil_parser_runtime_result_release(&runtime);
  puts("ABI-RUNTIME-ERROR-REUSE-OK"); fflush(stdout);
  return 0;
}
END-C
 )
 (define probe-reuse (c-lambda (unsigned-int64 UTF-8-string) int "probe_result_reuse")))

(def (main . _)
  (displayln "NATIVE-ABI-MODULES-LOADED") (force-output)
  (let (expected (rust-runtime-source
                 "t/fixtures/runtime-record-assignments/languages/records/grammar.ss"))
    (displayln "NATIVE-ABI-EXPECTED-GENERATED") (force-output)
    (let (handle (register-language! gql-language-grammar))
      (dynamic-wind void
        (lambda ()
          (let (status (probe-reuse handle expected))
            (unless (zero? status) (error "public ABI regression failed" status))))
        (lambda () (release-language! handle)))))
  (displayln "NATIVE-ABI-OK") (force-output))
