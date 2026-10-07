/* Independent process: real SDK setup/cleanup, never a gxi child exit shim. */
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <gerbil-parser/runtime.h>
#include <gerbil-parser/language-v2.h>
#include "t/fixtures/shared-scanner/records-native.h"
extern int gerbil_parser_rust_native_host_probe(void);
extern int gerbil_parser_rust_native_probe(void);
static double seconds(void) {
  struct timespec ts; clock_gettime(CLOCK_MONOTONIC,&ts);
  return ts.tv_sec + ts.tv_nsec * 1e-9;
}
static void *foreign_shutdown(void *argument) {
  (void)argument;
  return gerbil_parser_runtime_shutdown() == -1 ? NULL : (void *)1;
}
int main(int argc, char **argv) {
  if (argc == 2 && !strcmp(argv[1],"rust")) return gerbil_parser_rust_native_host_probe();
  puts("NATIVE-HOST-SETUP");fflush(stdout);
  double started = seconds();
  if (gerbil_parser_runtime_init()) return 1;
  printf("NATIVE-HOST-READY setup-ms=%.3f\n",1000*(seconds()-started));fflush(stdout);
  if (argc == 2 && !strcmp(argv[1],"borrowed")) {
    if (gerbil_parser_rust_native_probe()) return 12;
    if (gerbil_parser_runtime_shutdown()) return 13;
    puts("RUST-BORROWED-HOST-OK");fflush(stdout);
    return 0;
  }
  puts("NATIVE-HOST-DUPLICATE-INIT");fflush(stdout);
  if (gerbil_parser_runtime_init() != -3) return 2;
  puts("NATIVE-HOST-FOREIGN-THREAD");fflush(stdout);
  pthread_t foreign; void *thread_status;
  if (pthread_create(&foreign,NULL,foreign_shutdown,NULL) ||
      pthread_join(foreign,&thread_status) || thread_status) return 3;
  puts("NATIVE-HOST-LANGUAGE-CREATE");fflush(stdout);
  uint64_t handle = records_language_create();
  if (!handle) return 4;
  puts("NATIVE-HOST-HANDLE-BARRIER");fflush(stdout);
  if (gerbil_parser_runtime_shutdown() != -2) return 4;
  gerbil_parser_result_v2 result; gerbil_parser_result_v2_init(&result);
  const uint8_t source[] = "α=1\n";
  started = seconds();
  for (int i=0;i<100;i++) {
    if (gerbil_parser_language_parse(handle,source,sizeof(source)-1,&result) ||
        result.length<80 || result.payload[8]) return 5;
    if ((i+1)%25 == 0) {printf("NATIVE-HOST-PARSE-OK calls=%d\n",i+1);fflush(stdout);}
  }
  printf("NATIVE-HOST-100-CALLS wall-ms=%.3f\n",1000*(seconds()-started));fflush(stdout);
  if (gerbil_parser_language_release(handle)) return 6;
  /* The result alone must keep shutdown busy after the handle is released. */
  if (gerbil_parser_runtime_shutdown() != -2) return 7;
  gerbil_parser_result_v2_release(&result);
  puts("NATIVE-HOST-CLEANUP");fflush(stdout);
  if (gerbil_parser_runtime_shutdown()) return 8;
  if (gerbil_parser_language_is_owner_thread() || records_language_create()) return 9;
  if (gerbil_parser_language_parse(handle,source,sizeof(source)-1,&result) != -1 ||
      gerbil_parser_language_descriptor(handle,&result) != -1 ||
      gerbil_parser_language_release(handle) != -1) return 10;
  gerbil_parser_result_v2_release(&result);
  if (gerbil_parser_runtime_shutdown() != -3 || gerbil_parser_runtime_init() != -3) return 11;
  puts("NATIVE-HOST-CLEANUP-OK");fflush(stdout);
  return 0;
}
