;; Ordinary emitted Scheme, including a real native C dependency. The default
;; linker must resolve pthread symbols, rather than only pure generated Scheme.
(c-declare "#include <pthread.h>
static int parser_compiler_host_unit(void) {
  pthread_mutex_t mutex;
  if (pthread_mutex_init(&mutex, 0)) return 0;
  if (pthread_mutex_lock(&mutex)) return 0;
  if (pthread_mutex_unlock(&mutex)) return 0;
  return pthread_mutex_destroy(&mutex) == 0;
}")
(define compiler-host-unit (c-lambda () int "parser_compiler_host_unit"))
(define (compiler-host-probe value) (+ value (compiler-host-unit)))
