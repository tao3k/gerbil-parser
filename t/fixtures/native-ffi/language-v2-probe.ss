;;; Public byte-length C API with an independently compiled language pack.
(import :gerbil-parser/src/ffi/language-v2-native
        :gerbil-parser/t/fixtures/shared-scanner/records-native
        (only-in :gerbil-parser/src/ffi/language-handles
                 release-native-language! native-language-handle-parse)
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(export main parse-native-batch create-native-handle)
(extern probe-language parse-native-batch create-native-handle)
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/native-ffi/language-v2-probe#" probe-language parse-native-batch create-native-handle))
 (c-declare #<<END-C
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include "t/fixtures/shared-scanner/records-native.h"
#include <gerbil-parser/language-v2.h>
static int parse_native_batch(uint64_t handle) {
  const uint8_t source[] = "α=1\n";
  gerbil_parser_result_v2 result; gerbil_parser_result_v2_init(&result);
  for (int i=0;i<100;i++) {
    if (gerbil_parser_language_parse(handle,source,sizeof(source)-1,&result) || result.length<80 || result.payload[8]) {
      gerbil_parser_result_v2_release(&result);return -1;
    }
  }
  gerbil_parser_result_v2_release(&result);return 0;
}
static double monotonic_seconds(void) {
  struct timespec ts;clock_gettime(CLOCK_MONOTONIC,&ts);return ts.tv_sec + ts.tv_nsec * 1e-9;
}
static void *foreign_thread_probe(void *argument) {
  uint64_t handle = *(uint64_t *)argument;
  gerbil_parser_result_v2 r;gerbil_parser_result_v2_init(&r);
  int failed = gerbil_parser_language_is_owner_thread() != 0 || records_language_create() != 0
    || gerbil_parser_language_descriptor(handle,&r)!=-1
    || gerbil_parser_language_parse(handle,NULL,0,&r)!=-1
    || gerbil_parser_language_release(handle)!=-1;
  gerbil_parser_result_v2_release(&r);
  return failed ? (void *)argument : NULL;
}
static int probe_language(uint64_t handle) {
  gerbil_parser_result_v2 r;
  const uint8_t valid[] = {'a','=', '1','\n','!'};
  const uint8_t unicode[] = {0xce,0xb1,'=','1','\n'};
  const uint8_t nul[] = {'a','=','1',0,'!'};
  const uint8_t invalid[] = {0xff};
  if (gerbil_parser_language_abi_version() != 2) return 1;
  gerbil_parser_result_v2_init(&r);
  pthread_t foreign;void *thread_status;
  if (pthread_create(&foreign,NULL,foreign_thread_probe,&handle) || pthread_join(foreign,&thread_status) || thread_status) return 14;
  if (gerbil_parser_language_descriptor(handle,&r) || !r.payload || r.length==0) {
    /* Descriptor is length-delimited, never require a NUL terminator. */
    gerbil_parser_result_v2_release(&r); return 2;
  }
  double started=monotonic_seconds();clock_t cpu_started=clock();
  for (int i=0;i<100;i++) {
    if (gerbil_parser_language_parse(handle,valid,4,&r) || r.length<80 || r.payload[8]!=0) return 3;
    if ((i+1)%25==0) {printf("LANGUAGE-ABI-PARSE-OK calls=%d\n",i+1);fflush(stdout);}
  }
  printf("LANGUAGE-ABI-100-CALLS wall-ms=%.3f cpu-ms=%.3f\n",1000*(monotonic_seconds()-started),1000.0*(clock()-cpu_started)/CLOCKS_PER_SEC);fflush(stdout);
  if (gerbil_parser_language_parse(handle,unicode,sizeof unicode,&r) || r.payload[8]!=0) return 4;
  if (gerbil_parser_language_parse(handle,nul,sizeof nul,&r) || r.payload[8]!=1) return 5;
  if (gerbil_parser_language_parse(handle,invalid,sizeof invalid,&r)!=-1 || r.status!=-1) return 6;
  if (gerbil_parser_language_parse(handle,NULL,0,&r) || r.payload[8]!=0) return 7;
  if (gerbil_parser_language_parse(handle,NULL,1,&r)!=-1 || r.payload || r.length) return 8;
  if (gerbil_parser_language_parse(handle,valid,67108865,&r)!=-1) return 9;
  if (gerbil_parser_language_parse(handle,valid,4,NULL)!=-1) return 10;
  if (gerbil_parser_language_release(handle)) return 11;
  if (gerbil_parser_language_parse(handle,valid,4,&r)!=-1 || r.status!=-1) return 12;
  gerbil_parser_result_v2_release(&r);gerbil_parser_result_v2_release(&r);
  if (r.payload || r.length || r.status) return 13;
  gerbil_parser_result_v2_init(NULL);gerbil_parser_result_v2_release(NULL);
  return 0;
}
END-C
 )
 (define probe-language (c-lambda (unsigned-int64) int "probe_language"))
 (define parse-native-batch (c-lambda (unsigned-int64) int "parse_native_batch"))
 (define create-native-handle (c-lambda () unsigned-int64 "records_language_create")))
(def (main . _)
  (displayln "LANGUAGE-ABI-PACK-REGISTER") (force-output)
  (let* ((started (##current-time-point)) (handle (create-native-handle)))
    (when (zero? handle) (error "native language creation failed"))
    (displayln "LANGUAGE-ABI-CREATE wall-ms=" (* 1000 (- (##current-time-point) started))) (force-output)
    (unless (zero? (parse-native-batch handle)) (error "native warmup failed"))
    (##gc)
    (unless (thread-join! (thread-start!
                          (make-thread (lambda ()
                            (with-exception-catcher (lambda (_) #t)
                              (lambda () (native-language-handle-parse handle #u8()) #f))))))
      (error "foreign thread admitted"))
    (let (status (probe-language handle))
      (unless (zero? status) (error "language ABI probe failed" status)))
    (release-native-language! handle))
  (displayln "LANGUAGE-ABI-OK") (force-output)
  (test-child-process-exit! 0))
