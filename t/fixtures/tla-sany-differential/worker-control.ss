;;; A shared barrier proves two Workers execute in the same Scheme runtime.
(import (only-in :std/sync/barrier make-barrier barrier-post! barrier-wait!))
(export worker-control-loaded! worker-control-loaded-count worker-control-rendezvous!)
(def ready (make-barrier 2))
(def loaded 0)
(def (worker-control-loaded!) (set! loaded (+ loaded 1)))
(def (worker-control-loaded-count) loaded)
(def (worker-control-rendezvous!) (barrier-post! ready) (barrier-wait! ready))

(def runtime-thread #f)
(def (worker-control-runtime-owner!)
  (unless runtime-thread (set! runtime-thread (current-thread)))
  (eq? runtime-thread (current-thread)))
(export worker-control-runtime-owner!)
