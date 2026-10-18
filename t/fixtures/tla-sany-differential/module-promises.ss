;;; Observe the SDK's actual promise force, never pre-evaluate a source graph.
;;; Context mutation is confined to serialized admission / exclusive owners.
;;; No SDK global binding, module cache or failure state is replaced.
(def current-module-promise-admission (make-parameter #f))
(def +observed-module-promises+ (make-table test: eq?))

(def (observed-module-promise context original)
  (delay-atomic
    (let (admit (current-module-promise-admission))
      (if (not admit) (force original)
        (let ((name (gx#expander-context-id context))
              (runtime (module-evaluation-runtime-name context)))
          (displayln "MODULE-EVALUATE " name) (force-output)
          ;; At this point the SDK has already forced the prelude and reached
          ;; this exact compiled body. Closed wrapper admission preserves its
          ;; loader order; unsupported wrappers delegate unchanged.
          (when runtime (admit runtime))
          (call-with-values (lambda () (force original))
            (lambda results
              (displayln "MODULE-EVALUATED " name) (force-output)
              (apply values results))))))))

(def (install-module-promise-trace! context)
  (def (imports entries)
    (for-each
      (lambda (entry)
        (cond ((gx#module-context? entry) (visit entry))
              ((gx#import-set? entry) (visit (gx#import-set-source entry)))
              ((gx#module-import? entry)
               (visit (gx#module-export-context (gx#module-import-source entry))))))
      entries))
  (def (visit context)
    (when (and (or (gx#module-context? context) (gx#prelude-context? context))
               (not (table-ref +observed-module-promises+ context #f)))
      ;; Mark before graph traversal, including shared and cyclic references.
      ;; Walking phase-shifted imports installs no initializer effects. Actual
      ;; runtime phase selection remains entirely in SDK __eval-import.
      (table-set! +observed-module-promises+ context #t)
      (cond
        ((gx#module-context? context)
         (gx#module-context-e-set! context
           (observed-module-promise context (gx#module-context-e context)))
         (imports (gx#module-context-import context))
         (visit (gx#core-context-prelude context)))
        (else
         (gx#prelude-context-e-set! context
           (observed-module-promise context (gx#prelude-context-e context)))
         (imports (gx#prelude-context-import context))))))
  (visit context))
