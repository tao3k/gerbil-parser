;;; Real ASP sample completion notices, after timing and memory observation.
;;; Runtime binding wrappers return the exact original observation unchanged.
(import :gerbil/expander)
(export install-native-benchmark-progress!)
(def installed? #f)
(def (install-native-benchmark-progress!)
  (unless installed?
    (let ((owner (import-module ':asp-gerbil-scheme/src/benchmark/gate #f #t)) (sample 0))
      (for-each
       (lambda (name)
         ;; The pinned ASP source defines these runtime helpers privately.
         ;; Resolve the actual module-local binding rather than a public export.
         (let (binding (resolve-identifier name 0 owner))
           (unless binding (error "ASP sample observation seam is unavailable" name))
           (let* ((id (binding-id binding)) (original (eval id))
                  (observed (lambda (thunk)
                              (let* ((result (original thunk))
                                     (elapsed (if (pair? result) (car result) result)))
                                (set! sample (+ sample 1))
                                (write (list 'ASP-SAMPLE-OK name sample 'elapsed-nanos elapsed))
                                (newline) (force-output)
                                result))))
             (eval (list 'set! id (list 'quote observed))))))
       '(benchmark-result-attempt benchmark-elapsed-nanos/preconditioned)))
    (set! installed? #t)))
