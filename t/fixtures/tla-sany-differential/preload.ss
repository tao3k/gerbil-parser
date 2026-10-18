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

;; Source owners can finish interface expansion before evaluating their
;; dependency bodies. Observe that official phase too; never replace evaluation
;; or synthesize progress while a single evaluation is blocked.
(def (call-with-module-evaluation-trace thunk)
  (let (evaluate (gx#current-expander-module-eval))
    (parameterize
      ((gx#current-expander-module-eval
        (lambda (module)
          (let (name (if (or (gx#module-context? module) (gx#prelude-context? module))
                        (gx#expander-context-id module) module))
            (displayln "MODULE-EVALUATE " name) (force-output)
            (call-with-values
              (lambda () (evaluate module))
              (lambda results
                (displayln "MODULE-EVALUATED " name) (force-output)
                (apply values results)))))))
      (thunk))))

(def (import-compiled-interface! module)
  (call-with-compiled-interface-trace
   (lambda ()
     (gx#import-module (string->symbol (string-append ":" module)) #f #t))))

(def (preload-runtime-module module (visiting #f))
  ;; SDK states are authoritative, including loading and failed states.
  ;; Inspect wrappers only for a cold module so its dependencies report real
  ;; progress before the SDK initializes the entry. Warm entries never scan.
  (let (visiting (or visiting (make-table test: equal?)))
    (unless (table-ref visiting module #f)
      (table-set! visiting module #t)
      (unless (or (assoc module (list-modules)) (string-contains module "~"))
        (let (source (compiled-file module ".scm"))
          (when source
            (for-each
              (lambda (dependency) (preload-runtime-module dependency visiting))
              (call-with-input-file source
                (lambda (port)
                  (let loop ((dependencies '()))
                    (let (value (read port))
                      (if (eof-object? value) dependencies
                        (loop (append dependencies (compiled-dependencies value))))))))))))
      (displayln "MODULE-LOAD " module) (force-output)
      (load-module module)
      (displayln "MODULE-LOADED " module) (force-output))))

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
