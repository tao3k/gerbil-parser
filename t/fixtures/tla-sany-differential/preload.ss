(import (only-in :gerbil-parser/src/modules/loader
                 compiled-dependencies compiled-wrapper-forms read-compiled-dependencies
                 sdk-module-ready? settled-source-wrapper? owned-source-wrapper?
                 load-ready-source-wrapper load-parser-module)
        (only-in :gerbil/runtime/init __scheme-source))
;;; -*- Gerbil -*-
;;; Report real compiled-module admission before the test entry imports it.
;;; Dependency boundaries keep cold imports subject to the same silence gate.
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

;; Runtime wrappers do not list compile-time imports from .ssi interfaces.
;; Use the expander's import parameter to report the real nested admission
;; work. Delegate unchanged and restore the parameter on return or exception.
(def +reported-interface-imports+ (make-table test: equal?))
(def current-interface-tracing? (make-parameter #f))
(def (interface-request-key path)
  ;; A relative request can select a different module in another caller.
  ;; Cache diagnostics only for resolved files and qualified library paths.
  (let (value (gx#stx-e path))
    (def (qualified? value)
      (and (symbol? value) (string-prefix? ":" (symbol->string value))))
    (cond ((qualified? value) value)
          ((and (string? value) (string-prefix? "/" value)) value)
          ((and (pair? value) (list? value) (qualified? (gx#stx-e (car value)))
                (andmap (lambda (part)
                          (let (datum (gx#stx-e part))
                            (or (symbol? datum) (string? datum)))) value))
           (map gx#stx-e value))
          (else #f))))
(def (call-with-compiled-interface-trace thunk)
  (if (current-interface-tracing?) (thunk)
    (let (importer (gx#current-expander-module-import))
      (parameterize
          ((current-interface-tracing? #t)
           (gx#current-expander-module-import
            (lambda (path reload?)
              (let* ((key (interface-request-key path))
                     (report? (or reload? (not key)
                                  (not (table-ref +reported-interface-imports+ key #f)))))
                (when report? (displayln "INTERFACE-IMPORT " path) (force-output))
                (with-catch
                  (lambda (exception)
                    (when key (table-set! +reported-interface-imports+ key #f))
                    (displayln "INTERFACE-IMPORT-FAILED " path) (force-output)
                    (raise exception))
                  (lambda ()
                    ;; Every lookup still runs, including cached diagnostics.
                    (let (context (importer path reload?))
                      (when (current-module-promise-admission)
                        (install-module-promise-trace! context))
                      (when key (table-set! +reported-interface-imports+ key #t))
                      (when report? (displayln "INTERFACE-IMPORTED " path) (force-output))
                      context)))))))
        (thunk)))))

;; Source owners can finish interface expansion before evaluating their
;; dependency bodies. Observe that official phase too; never replace evaluation
;; or synthesize progress while a single evaluation is blocked.
;;; A macro-generated import is not present in the author's raw import forms.
;;; Admit only the compiled context the SDK is actually about to evaluate.
(def (module-evaluation-runtime-name module)
  (and (gx#module-context? module)
       (let ((path (gx#module-context-path module))
             (id (gx#expander-context-id module)))
         (and (string? path) (string-suffix? ".ssi" path)
              (let (name (cond ((symbol? id) (symbol->string id))
                              ((string? id) id) (else #f)))
                (and name
                     (let ((interface (compiled-file name ".ssi"))
                           (wrapper (compiled-file name ".scm")))
                       (and interface wrapper
                            (equal? (path-normalize path) (path-normalize interface))
                            (equal? (path-normalize wrapper)
                                    (path-normalize
                                      (string-append (path-strip-extension interface) ".scm")))
                            name))))))))

(include "module-promises.ss")

(def (evaluate-observed-module module evaluate (admit preload-runtime-module))
  ;; Install observers without forcing imports. The SDK still chooses when to
  ;; force each promise, including nested source helpers and opaque bodies.
  (install-module-promise-trace! module)
  (parameterize ((current-module-promise-admission admit))
    (evaluate module)))

(def (call-with-module-evaluation-trace thunk)
  (let (evaluate (gx#current-expander-module-eval))
    (parameterize
      ((current-module-promise-admission preload-runtime-module)
       (gx#current-expander-module-eval
        (lambda (module) (evaluate-observed-module module evaluate))))
      (call-with-compiled-interface-trace thunk))))

(def (import-compiled-interface! module)
  (call-with-compiled-interface-trace
   (lambda ()
     (gx#import-module (string->symbol (string-append ":" module)) #f #t))))

(def (preload-runtime-module module (visiting #f))
  (load-parser-module module visiting
    (lambda (phase name)
      (displayln (if (eq? phase 'loading) "MODULE-LOAD " "MODULE-LOADED ") name)
      (force-output))))

(def (preload-module module)
  ;; Runtime dependencies need native initialization, not an expander import
  ;; for every leaf. The SDK owns the requested interface's metadata graph.
  (preload-runtime-module module)
  (when (compiled-file module ".ssi")
    (displayln "MODULE-IMPORT " module) (force-output)
    (import-compiled-interface! module)
    (displayln "MODULE-IMPORTED " module) (force-output)))

;; Admit compiled dependencies declared by tests and their source helpers.
;; Reading import forms does not evaluate a helper or replace gxtest's owner.
(def +preloaded-sources+ (make-table test: equal?))

;;; Relative symbol and string imports have the same native ownership as colon
;;; imports. Derive the module identity from the nearest actual package manifest.
(def (source-module-name file)
  (let ((file (path-normalize file)))
    (let loop ((directory (path-directory file)))
      (let (manifest (path-expand "gerbil.pkg" directory))
        (cond
         ((file-exists? manifest)
          (let* ((declaration (call-with-input-file manifest read))
                 (package (member 'package: declaration)))
            (and package (pair? (cdr package)) (symbol? (cadr package))
                 (string-append (symbol->string (cadr package)) "/"
                   (path-strip-extension
                    (substring file (string-length directory) (string-length file)))))))
         ((equal? directory "/") #f)
         (else (loop (path-directory
                      (substring directory 0 (- (string-length directory) 1))))))))))

(def (preload-relative-import value source)
  (let (file (path-normalize
              (path-expand
               (if (string-suffix? ".ss" value) value (string-append value ".ss"))
               (path-directory source))))
    (when (file-exists? file)
      (let (module (source-module-name file))
        (if (and module (compiled-file module ".ssi"))
          (preload-module module)
          (preload-test-imports file))))))

(def (preload-import-set value source)
  (cond
   ((symbol? value)
    (let (name (symbol->string value))
      (cond
       ((string-prefix? ":" name)
        (let (module (substring name 1 (string-length name)))
          ;; Source-only namespace imports need the same dependency walk as
          ;; relative source imports. Never admit an interface-less wrapper.
          (cond
           ((compiled-file module ".ssi") (preload-module module))
           ((compiled-file module ".ss") => preload-test-imports))))
       ((or (string-prefix? "./" name) (string-prefix? "../" name))
        (preload-relative-import name source)))))
   ((string? value)
    (preload-relative-import value source))
   ((pair? value)
    (for-each (lambda (entry) (preload-import-set entry source)) value))))

(def (preload-test-imports file)
  (let (source (path-expand file))
    (unless (table-ref +preloaded-sources+ source #f)
      ;; The in-progress marker bounds cyclic source imports. Failed admission
      ;; removes it so a fixed file can be inspected on the next attempt.
      (table-set! +preloaded-sources+ source #t)
      (with-catch
        (lambda (exception)
          (table-set! +preloaded-sources+ source #f)
          (raise exception))
        (lambda ()
          (call-with-input-file source
            (lambda (port)
              (let loop ()
                (let (form (read port))
                  (unless (eof-object? form)
                    (when (and (pair? form) (eq? (car form) 'import))
                      (for-each (lambda (entry) (preload-import-set entry source))
                                (cdr form)))
                    (loop)))))))))))
