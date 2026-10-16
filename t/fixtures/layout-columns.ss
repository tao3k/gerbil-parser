;;; Boxed-vector oracle, independent of packed storage and lookup dispatch.
(export host-slot-control host-mutex-control host-spin-backend host-max-processors closure-frame-roots wide-frame-roots reference-layout-columns reference-packed-layout-columns layout-column-sources)

(def (reference-layout-columns source)
  (let* ((bytes (string->utf8 source)) (size (u8vector-length bytes))
         (columns (make-vector (+ size 1) 0)))
    (let loop ((offset 0) (column 0))
      (if (= offset size)
        (begin (vector-set! columns offset column) columns)
        (let* ((byte (u8vector-ref bytes offset))
               (width (cond ((< byte 128) 1) ((< byte 224) 2)
                            ((< byte 240) 3) (else 4)))
               (end (+ offset width)))
          (let fill ((cursor offset))
            (when (< cursor end)
              (vector-set! columns cursor column) (fill (+ cursor 1))))
          (loop end (cond ((or (= byte 10) (= byte 13)) 0)
                          ((= byte 9) (* (+ (quotient column 8) 1) 8))
                          (else (+ column 1)))))))))

(def layout-column-sources
  (list "" "ascii" "\t\t" "1234567\tX\t" "a\r\nb\rc\nd"
        "éλ中😀\tZ\r\n名字\tend"
        (list->string (map integer->char '(0 1 127 128 2047 2048 55295 57344 65535 65536 1114111)))))

;;; Matched former packed producer, independent of the ASCII-specialized writer.
;;; Keep the same capacity proof and private native setter as production.
(def (reference-packed-layout-columns source)
  (if (> (string-length source) (quotient #xffffffff 8))
    (reference-layout-columns source)
    (let* ((bytes (string->utf8 source)) (size (u8vector-length bytes))
           (columns (make-u32vector (+ size 1) 0)))
      (let loop ((offset 0) (column 0))
        (if (= offset size)
          (begin (##u32vector-set! columns offset column) columns)
          (let* ((byte (u8vector-ref bytes offset))
                 (width (cond ((< byte 128) 1) ((< byte 224) 2) ((< byte 240) 3) (else 4)))
                 (end (+ offset width))
                 (next-column (case byte ((10 13) 0)
                                ((9) (* (+ (quotient column 8) 1) 8))
                                (else (+ column 1)))))
            (let fill ((cursor offset))
              (when (< cursor end)
                (##u32vector-set! columns cursor column) (fill (+ cursor 1))))
            (loop end next-column)))))))

;;; Host GC map control: the opaque callback keeps 96 independent roots live.
(defsyntax (with-wide-frame-roots stx)
  (syntax-case stx ()
    ((_ seed collect)
     (let loop ((index 0) (names '()) (indices '()))
       (if (= index 96)
         (with-syntax (((root ...) (reverse names))
                       ((ordinal ...) (reverse indices)))
           #'(let ((root (cons ordinal seed)) ...)
               (collect)
               (list root ...)))
         (loop (+ index 1) (cons (genident 'live-root) names)
               (cons (datum->syntax #'with-wide-frame-roots index) indices)))))))

(def (wide-frame-roots seed collect)
  (with-wide-frame-roots seed collect))

;;; The returned closures are the only post-callback owners of these pairs.
;;; Two environments and an opaque callback exercise future-use propagation.
(def (closure-frame-roots seed collect)
  (let* ((left (cons 'left seed)) (right (cons 'right seed))
         (read-left (lambda () left)) (read-right (lambda () right)))
    (collect)
    (cons read-left read-right)))

;;; Host macro controls execute in the same compiled fixture as the GC controls.
;;; The OS waiter touches only C-owned words, never Scheme values or a VM.
(extern host-slot-control host-mutex-control host-spin-backend host-max-processors)
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/layout-columns#"
             host-slot-control host-mutex-control host-spin-backend host-max-processors))
 (c-declare #<<END-C
#include <stdlib.h>
static int parser_host_slot_control(void) {
  ___WORD words[256];
  ___WORD *base = words + 160;
  for (int delta = 0; delta < 128; ++delta) {
    ___WORD *writer = base;
    ___FP_ADJFP(writer, delta)
    *base = ___FIX(999);
    for (int slot = 1; slot <= 96; ++slot) {
      ___FP_SET_STK(writer, slot - delta, ___FIX(slot))
    }
    ___WORD *scanner = writer;
    ___FP_ADJFP(scanner, -delta)
    if (scanner != base || *base != ___FIX(999)) return 1;
    for (int slot = 1; slot <= 96; ++slot) {
      if (&___FP_STK(writer, slot - delta) != &___FP_STK(scanner, slot)) return 2;
      if (___FP_STK(scanner, slot) != ___FIX(slot)) return 3;
    }
  }
  return 0;
}
#ifdef ___USE_POSIX_THREAD_SYSTEM
struct parser_host_handoff {
  ___MUTEX lock;
  ___WORD payload[32];
  int status;
};
static void *parser_host_waiter(void *argument) {
  struct parser_host_handoff *state = argument;
  if (!___MUTEX_LOCK(state->lock)) { state->status = 4; return NULL; }
  for (int i = 0; i < 32; ++i)
    if (state->payload[i] != ___FIX(i + 1)) state->status = 5;
  if (!___MUTEX_UNLOCK(state->lock)) state->status = 6;
  return NULL;
}
static int parser_host_mutex_control(void) {
  struct parser_host_handoff state;
  pthread_t waiter;
  state.status = 0;
  for (int i = 0; i < 32; ++i) state.payload[i] = 0;
  if (!___MUTEX_INIT(state.lock)) return 1;
  if (!___MUTEX_LOCK(state.lock)) { ___MUTEX_DESTROY(state.lock); return 2; }
  if (pthread_create(&waiter, NULL, parser_host_waiter, &state)) {
    ___MUTEX_UNLOCK(state.lock); ___MUTEX_DESTROY(state.lock); return 3;
  }
  /* Writes follow pthread_create: thread creation cannot publish these values. */
  for (int i = 0; i < 32; ++i) state.payload[i] = ___FIX(i + 1);
  if (!___MUTEX_UNLOCK(state.lock)) abort();
  if (pthread_join(waiter, NULL)) abort();
  if (!___MUTEX_DESTROY(state.lock)) return 9;
  return state.status;
}
#else
static int parser_host_mutex_control(void) { return -1; }
#endif
static int parser_host_spin_backend(void) {
#ifdef ___SINGLE_THREADED_VMS
  return 0;
#elif defined(___USE_MUTEX_FOR_SPINLOCK)
  return 1;
#else
  return 2;
#endif
}
END-C
 )
 (define host-slot-control (c-lambda () int "parser_host_slot_control"))
 (define host-mutex-control (c-lambda () int "parser_host_mutex_control"))
 (define host-spin-backend (c-lambda () int "parser_host_spin_backend"))
 (define host-max-processors (c-lambda () int "___result = ___MAX_PROCESSORS;")))
