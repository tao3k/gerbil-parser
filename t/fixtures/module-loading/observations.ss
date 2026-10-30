;;; Diagnostic observation only. Gerbil owns selection, imports, evaluation,
;;; initialization, cache state and errors; these functions grant no admission.
(import (only-in :gerbil/expander current-expander-module-import
                 current-expander-module-eval import-module expander-context-id))

(def (call-with-compiled-interface-trace thunk)
  (let (importer (current-expander-module-import))
    (parameterize ((current-expander-module-import
      (lambda (path reload?)
        (displayln "INTERFACE-IMPORT " path) (force-output)
        (with-catch
          (lambda (exception)
            (displayln "INTERFACE-IMPORT-FAILED " path) (force-output)
            (raise exception))
          (lambda ()
            (let (context (importer path reload?))
              (displayln "INTERFACE-IMPORTED " path) (force-output)
              context))))))
      (thunk))))

(def (evaluate-observed-module module evaluate)
  (displayln "MODULE-EVALUATE " (expander-context-id module)) (force-output)
  (call-with-values (lambda () (evaluate module))
    (lambda results
      (displayln "MODULE-EVALUATED " (expander-context-id module)) (force-output)
      (apply values results))))

(def (call-with-module-evaluation-trace thunk)
  (let (evaluate (current-expander-module-eval))
    (parameterize ((current-expander-module-eval
      (lambda (module) (evaluate-observed-module module evaluate))))
      (call-with-compiled-interface-trace thunk))))

(def (preload-runtime-module module)
  (displayln "MODULE-LOAD " module) (force-output)
  (let (result (load-module module))
    (displayln "MODULE-LOADED " module) (force-output)
    result))

(def (preload-module module)
  ;; Metadata import and runtime initialization are distinct native operations.
  ;; No path probing, dependency walk, promise mutation or loaded-state table.
  (call-with-compiled-interface-trace
    (lambda () (import-module (string->symbol (string-append ":" module)))))
  (preload-runtime-module module))
