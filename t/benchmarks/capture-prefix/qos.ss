;;; Test-only host scheduling normalization; never a parser runtime policy.
(import :std/ffi)
(export admit-capture-qos!)
(extern set-capture-qos)
(begin-foreign
 (namespace ("gerbil-parser/t/benchmarks/capture-prefix/qos#" set-capture-qos))
 (c-declare #<<C
#ifdef __APPLE__
#include <pthread.h>
#include <pthread/qos.h>
static int gp_capture_qos(void) {
  qos_class_t actual;
  int priority;
  if (pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0) != 0) return 0;
  return pthread_get_qos_class_np(pthread_self(), &actual, &priority) == 0
      && actual == QOS_CLASS_USER_INTERACTIVE && priority == 0;
}
#else
static int gp_capture_qos(void) { return 1; }
#endif
C
 )
 (define set-capture-qos (c-lambda () int "gp_capture_qos")))
(def (admit-capture-qos!)
  (unless (= (set-capture-qos) 1) (error "capture benchmark host QoS admission failed")))
