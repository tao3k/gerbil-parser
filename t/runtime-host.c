/* Independent process: real SDK setup/cleanup, never a gxi child exit shim. */
#ifdef __APPLE__
#define _DARWIN_C_SOURCE 1
#endif
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <sys/resource.h>
#ifdef __APPLE__
#include <pthread/qos.h>
#endif
#include <gerbil-parser/runtime.h>
#include <gerbil-parser/language.h>
#include "t/fixtures/shared-scanner/records-abi.h"
extern int gerbil_parser_rust_native_host_probe(void);
extern int gerbil_parser_rust_tokio_host_probe(void);
extern int gerbil_parser_rust_tokio_cpu_probe(int unicode);
int gerbil_parser_benchmark_prepare_thread(void) {
#ifdef __APPLE__
  qos_class_t before, after; int relative;
  if (pthread_get_qos_class_np(pthread_self(), &before, &relative)) return -1;
  if (pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0)) return -1;
  if (pthread_get_qos_class_np(pthread_self(), &after, &relative)) return -1;
  printf("RUST-TOKIO-CPU-QOS before=%u after=%u relative=%d\n", before, after, relative);
  fflush(stdout);
  return after == QOS_CLASS_USER_INTERACTIVE && relative == 0 ? 0 : -1;
#else
  return 0;
#endif
}
double gerbil_parser_benchmark_cpu_seconds(void) {
  struct rusage usage;
  if (getrusage(RUSAGE_SELF, &usage)) return -1;
  return usage.ru_utime.tv_sec + usage.ru_utime.tv_usec * 1e-6 +
         usage.ru_stime.tv_sec + usage.ru_stime.tv_usec * 1e-6;
}
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
  if (argc == 2 && !strcmp(argv[1],"tokio")) return gerbil_parser_rust_tokio_host_probe();
  if (argc == 2 && !strcmp(argv[1],"tokio-cpu-ascii")) return gerbil_parser_rust_tokio_cpu_probe(0);
  if (argc == 2 && !strcmp(argv[1],"tokio-cpu-unicode")) return gerbil_parser_rust_tokio_cpu_probe(1);
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
  gerbil_parser_result result; gerbil_parser_result_init(&result);
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
  gerbil_parser_result_release(&result);
  puts("NATIVE-HOST-CLEANUP");fflush(stdout);
  if (gerbil_parser_runtime_shutdown()) return 8;
  if (gerbil_parser_language_is_owner_thread() || records_language_create()) return 9;
  if (gerbil_parser_language_parse(handle,source,sizeof(source)-1,&result) != -1 ||
      gerbil_parser_language_descriptor(handle,&result) != -1 ||
      gerbil_parser_language_release(handle) != -1) return 10;
  gerbil_parser_result_release(&result);
  if (gerbil_parser_runtime_shutdown() != -3 || gerbil_parser_runtime_init() != -3) return 11;
  puts("NATIVE-HOST-CLEANUP-OK");fflush(stdout);
  return 0;
}
