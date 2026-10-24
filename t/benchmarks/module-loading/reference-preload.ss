;;; Frozen pre-change preloader; benchmark control only.
(def (make-reference-preloader)
;;; -*- Gerbil -*-
;;; Report real compiled-module admission before the test entry imports it.
;;; Dependency boundaries keep cold imports subject to the same silence gate.
(def +preloaded-modules+ (make-table test: equal?))
(def +compiled-files+ (make-table test: equal?))

(def (prefer-compiled-interfaces!)
  ;; Called only by the native suite; source benchmark entries retain their
  ;; source overlay. FFI C forms require the compiled module interface.
  (let (compiled-root (getenv "GERBIL_PATH" #f))
    (when compiled-root
      (let (library (path-expand "lib" compiled-root))
        (when (file-exists? library)
          (add-load-path! library))))))

(def (compiled-file module suffix)
  ;; Native products are admitted before the child starts. Cache successful
  ;; lookups only, so a later load-path addition can still resolve a miss.
  (let (key (cons module suffix))
    (or (table-ref +compiled-files+ key #f)
        (let (path (find file-exists?
                        (map (lambda (root)
                               (path-expand (string-append module suffix) root))
                             (cons (path-expand "lib" (gerbil-home)) (load-path)))))
          (when path (table-set! +compiled-files+ key path))
          path))))

(def (compiled-dependencies value)
  (cond ((and (pair? value) (eq? (car value) 'load-module)
              (pair? (cdr value)) (string? (cadr value)))
         (list (cadr value)))
        ((pair? value)
         (append (compiled-dependencies (car value))
                 (compiled-dependencies (cdr value))))
        (else '())))

;; Runtime wrappers do not list compile-time imports from .ssi interfaces.
;; Use the expander's import parameter to report the real nested admission
;; work. Delegate unchanged and restore the parameter on return or exception.
(def (call-with-compiled-interface-trace thunk)
  (let (importer (gx#current-expander-module-import))
    (parameterize
        ((gx#current-expander-module-import
          (lambda (path reload?)
            (displayln "INTERFACE-IMPORT " path) (force-output)
            (let (context (importer path reload?))
              (displayln "INTERFACE-IMPORTED " path) (force-output)
              context))))
      (thunk))))

(def (import-compiled-interface! module)
  (call-with-compiled-interface-trace
   (lambda ()
     (gx#import-module (string->symbol (string-append ":" module)) #f #t))))

(def (preload-compiled-module! module interfaces?)
  (let (key (cons module interfaces?))
  (unless (table-ref +preloaded-modules+ key #f)
    (table-set! +preloaded-modules+ key #t)
    ;; Generated wrapper modules list dependencies without evaluating their
    ;; initializers. Phase bodies (~0/~1) are admitted directly, never decoded.
    (unless (string-contains module "~")
      (let (source (compiled-file module ".scm"))
        (when source
          (for-each (lambda (dependency) (preload-compiled-module! dependency interfaces?))
                    (call-with-input-file source
                      (lambda (port)
                        (let loop ((dependencies '()))
                          (let (value (read port))
                            (if (eof-object? value) dependencies
                              (loop (append dependencies (compiled-dependencies value))))))))))))
    (displayln "MODULE-LOAD " module) (force-output)
    (load-module module)
    (displayln "MODULE-LOADED " module) (force-output)
    ;; Admit phase bindings incrementally as well. Deferring all .ssi imports
    ;; until gxtest loads its first source module hides cold macro work.
    (when (and interfaces? (not (string-contains module "~"))
               (compiled-file module ".ssi"))
      (displayln "MODULE-IMPORT " module) (force-output)
      (import-compiled-interface! module)
      (displayln "MODULE-IMPORTED " module) (force-output)))))

(def (preload-module module) (preload-compiled-module! module #t))

;;; Compiled benchmark entries execute phase-zero procedures only. Loading
;;; source expander interfaces would retain unrelated compiler state in their
;;; runtime heap; dependency initialization still reports actual progress.
(def (preload-runtime-module module) (preload-compiled-module! module #f))


  preload-compiled-module!)
