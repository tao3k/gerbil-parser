;;; Keep the untimed interpreter/compiler closure out of the native product.
;;; The existing qualification protocol and complete tape comparison stay intact.
(import :gerbil-parser/t/benchmarks/parser-throughput/protocol
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)

(def (main corpus-path digest)
  (unless (eq? (bind-parse-org-compiled-events digest) parse-org-compiled-events)
    (error "direct real Fold binding mismatch"))
  (let (corpus (call-with-input-file corpus-path read))
    (unless (and (list? corpus) (= (length corpus) 4)
                 (eq? (car corpus) 'real-fold-preflight.v1)
                 (equal? (cadr corpus) digest)
                 (vector? (caddr corpus)) (vector? (cadddr corpus))
                 (= (vector-length (caddr corpus)) 1000)
                 (= (vector-length (cadddr corpus)) 1000))
      (error "invalid independently prepared preflight corpus"))
    (let ((inputs (caddr corpus)) (expected (cadddr corpus)) (oracle-index 0))
      (run-parser-throughput "upstream-real-org-fold-oev1-direct-prepared" digest
        parse-org-compiled-events
        (lambda (source)
          (unless (and (< oracle-index 1000)
                       (equal? source (vector-ref inputs oracle-index)))
            (error "prepared oracle input ordering mismatch" oracle-index))
          (let (result (vector-ref expected oracle-index))
            (set! oracle-index (+ oracle-index 1)) result))
        (lambda (index) (vector-ref inputs (- index 1000)))
        'preflight))))
