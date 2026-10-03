;;; -*- Gerbil -*-
;;; Report real compiled-module admission before the test entry imports it.
;;; Dependency boundaries keep cold imports subject to the same silence gate.
(def +preloaded-modules+ '())

(def (prefer-native-interfaces!)
  ;; Called only by the native suite; source benchmark entries retain their
  ;; source overlay. FFI C forms require the compiled module interface.
  (let (native-root (getenv "GERBIL_PATH" #f))
    (when native-root
      (let (library (path-expand "lib" native-root))
        (when (file-exists? library)
          (add-load-path! library))))))

(def (compiled-file module suffix)
  (find file-exists?
        (map (lambda (root) (path-expand (string-append module suffix) root))
             (cons (path-expand "lib" (gerbil-home)) (load-path)))))

(def (compiled-dependencies value)
  (cond ((and (pair? value) (eq? (car value) 'load-module)
              (pair? (cdr value)) (string? (cadr value)))
         (list (cadr value)))
        ((pair? value)
         (append (compiled-dependencies (car value))
                 (compiled-dependencies (cdr value))))
        (else '())))

(def (preload-module module)
  (unless (member module +preloaded-modules+)
    (set! +preloaded-modules+ (cons module +preloaded-modules+))
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
      (gx#import-module (string->symbol (string-append ":" module)) #f #t)
      (displayln "MODULE-IMPORTED " module) (force-output))))

;; Admit the compiled dependencies named by this test's import declarations.
;; Source forms are read as data; unrelated language packs stay outside the
;; test process. Relative test helper imports remain with gxtest's source owner.
(def (preload-import-set value)
  (cond ((symbol? value)
         (let (name (symbol->string value))
           (when (string-prefix? ":" name)
             (let (module (substring name 1 (string-length name)))
               (when (or (compiled-file module ".ssi")
                         (compiled-file module ".scm"))
                 (preload-module module))))))
        ((pair? value) (for-each preload-import-set value))))

(def (preload-test-imports file)
  (call-with-input-file file
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (unless (eof-object? form)
            (when (and (pair? form) (eq? (car form) 'import))
              (for-each preload-import-set (cdr form)))
            (loop)))))))
