;;; Test-only host scheduling normalization; never a parser runtime policy.
(import :std/ffi)
(export admit-benchmark-qos!)
(extern set-benchmark-qos)
(begin-foreign
 (namespace ("gerbil-parser/t/support/qualification/benchmark-qos#" set-benchmark-qos))
 (c-declare #<<C
#ifdef __APPLE__
#include <pthread.h>
#include <pthread/qos.h>
static int gp_benchmark_qos(void) {
  qos_class_t actual;
  int priority;
  if (pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0) != 0) return 0;
  return pthread_get_qos_class_np(pthread_self(), &actual, &priority) == 0
      && actual == QOS_CLASS_USER_INTERACTIVE && priority == 0;
}
#else
static int gp_benchmark_qos(void) { return 1; }
#endif
C
 )
 (define set-benchmark-qos (c-lambda () int "gp_benchmark_qos")))
(def (admit-benchmark-qos!)
  (unless (= (set-benchmark-qos) 1) (error "benchmark host QoS admission failed")))
