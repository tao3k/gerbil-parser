;;; -*- Gerbil -*-
;;; One request-owned scanner result, reusable only in its exact lexical context.
(import (only-in ./lexer scan-source-token))
(export current-lr-lexical-plan-reuse-enabled? make-source-probe-cache source-probe-scan source-probe-take!)

;;; Complete-edit controls favor some flat prefixes, but nested and history
;;; gains are not stable. Keep stronger lexical reuse an explicit experiment.
(def current-lr-lexical-plan-reuse-enabled? (make-parameter #f))

(defstruct source-probe-cache-instance (machine source slot scanner))
(def (make-source-probe-cache machine source (scanner #f))
  (make-source-probe-cache-instance machine source #f scanner))

;;; An internal source owner may provide a scanner that returns independently
;;; certified old tokens; the slot still binds the actual requested mode.
;;; A failed speculative read leaves no cached error or prior stale result.
;;; The canonical lexer remains responsible for diagnostics.
(def (source-probe-scan cache character byte mode)
  (source-probe-cache-instance-slot-set! cache #f)
  (let-values (((token next-character)
                (if (source-probe-cache-instance-scanner cache)
                  ((source-probe-cache-instance-scanner cache) character byte mode)
                  (scan-source-token (source-probe-cache-instance-machine cache)
                                     (source-probe-cache-instance-source cache)
                                     character byte mode))))
    (source-probe-cache-instance-slot-set! cache
      (vector character byte mode token next-character))
    (values token next-character)))

;;; Modes are interned in one prepared runtime. Object identity is stronger
;;; than comparing the mode's numeric ID, which another runtime can also use.
;;; A future-position result survives intervening trivia; consumed/passed results
;;; are dropped. The cache never grows with source size or edit history.
(def (source-probe-take! cache character byte mode)
  (let (slot (source-probe-cache-instance-slot cache))
    (and slot
         (cond
          ((and (= character (vector-ref slot 0)) (= byte (vector-ref slot 1))
                (eq? mode (vector-ref slot 2)))
           (source-probe-cache-instance-slot-set! cache #f)
           (cons (vector-ref slot 3) (vector-ref slot 4)))
          ((> byte (vector-ref slot 1))
           (source-probe-cache-instance-slot-set! cache #f) #f)
          (else #f)))))
