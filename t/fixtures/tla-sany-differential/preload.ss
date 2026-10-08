;;; -*- Gerbil -*-
;;; Report real compiled-module admission before the test entry imports it.
;;; Dependency boundaries keep cold imports subject to the same silence gate.
(def +preloaded-modules+ (make-table test: equal?))
(def +compiled-files+ (make-table test: equal?))

(def (prefer-native-interfaces!)
  ;; Called only by the native suite; source benchmark entries retain their
  ;; source overlay. FFI C forms require the compiled module interface.
  (let (native-root (getenv "GERBIL_PATH" #f))
    (when native-root
      (let (library (path-expand "lib" native-root))
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
(def (call-with-native-interface-trace thunk)
  (let (importer (gx#current-expander-module-import))
    (parameterize
        ((gx#current-expander-module-import
          (lambda (path reload?)
            (displayln "INTERFACE-IMPORT " path) (force-output)
            (let (context (importer path reload?))
              (displayln "INTERFACE-IMPORTED " path) (force-output)
              context))))
      (thunk))))

(def (import-native-interface! module)
  (call-with-native-interface-trace
   (lambda ()
     (gx#import-module (string->symbol (string-append ":" module)) #f #t))))

(def (preload-module module)
  (unless (table-ref +preloaded-modules+ module #f)
    (table-set! +preloaded-modules+ module #t)
    ;; Generated wrapper modules list dependencies without evaluating their
    ;; initializers. Phase bodies (~0/~1) are admitted directly, never decoded.
    (unless (string-contains module "~")
      (let (source (compiled-file module ".scm"))
        (when source
          (for-each preload-module
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
    (when (and (not (string-contains module "~"))
               (compiled-file module ".ssi"))
      (displayln "MODULE-IMPORT " module) (force-output)
      (import-native-interface! module)
      (displayln "MODULE-IMPORTED " module) (force-output))))

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
          ;; A runtime wrapper without its interface is an incomplete build
          ;; product. Let Gerbil import source instead of admitting that wrapper.
          (when (compiled-file module ".ssi")
            (preload-module module))))
       ((or (string-prefix? "./" name) (string-prefix? "../" name))
        (preload-relative-import name source)))))
   ((string? value)
    (preload-relative-import value source))
   ((pair? value)
    (for-each (lambda (entry) (preload-import-set entry source)) value))))

(def (preload-test-imports file)
  (let (source (path-expand file))
    (unless (table-ref +preloaded-sources+ source #f)
      (table-set! +preloaded-sources+ source #t)
      (call-with-input-file source
        (lambda (port)
          (let loop ()
            (let (form (read port))
              (unless (eof-object? form)
                (when (and (pair? form) (eq? (car form) 'import))
                  (for-each (lambda (entry) (preload-import-set entry source))
                            (cdr form)))
                (loop)))))))))
