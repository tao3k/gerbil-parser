;; Ordinary emitted Scheme, compiled without per-job C options just like a
;; Gerbil macro-phase job. This builds a parser test fixture, never the SDK.
(define (compiler-host-probe value) (+ value 1))
