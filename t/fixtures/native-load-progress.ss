;;; Real compiled-module loads occur inside expander evaluation. Report each
;;; operation so the strict silence gate can see nested dependency loading.
(import :gerbil/expander)
(export install-native-load-progress!)
(def (install-native-load-progress!)
  (let* ((context (import-module ':gerbil/runtime/loader #f #t))
         (entry (find (lambda (entry) (eq? (module-export-name entry) 'load-module))
                      (module-context-export context)))
         (binding (binding-id (core-resolve-module-export entry)))
         (loader (eval binding)))
    (eval (list 'set! binding
                (list 'quote
                      (lambda (path)
                        (displayln "LOAD " path) (force-output)
                        (let (result (loader path))
                          (displayln "LOAD-OK " path " file=" result) (force-output)
                          result)))))))
