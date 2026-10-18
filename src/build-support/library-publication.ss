;;; std/make owns in-place compilation and cleaning. Readers admit only a
;;; completed, source-bound generation under the same reader/writer lock.
(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/os/flock open-output-file/lock LOCK_SH)
        (only-in :std/os/device device-close)
        "./preparation-snapshot.ss" "./compiler-profile.ss"
        "./build-environment.ss"
        "./loader-products.ss")
(export call-with-library-publication library-publication-inputs publication-build-main call-with-library-reader)

(def (publication-root path)
  (let loop ((path (path-normalize (path-expand path))))
    (if (and (> (string-length path) 1) (string-suffix? "/" path))
      (loop (substring path 0 (- (string-length path) 1))) path)))

(def (library-publication-inputs root)
  (let* ((library (path-expand "lib" root))
         (home (gerbil-home))
         (sources
          (append (apply append (map preparation-files '("src" "languages" "language-support" "include")))
                  (filter (lambda (path) (string-suffix? ".ss" path))
                          (list-sort string<? (directory-files ".")))
                  '("gerbil.pkg")))
         (namespace (path-expand "gerbil-parser" library))
         (foreign
          ;; Classify derived files after traversal: a directory named *.c or
          ;; *.o can contain real interfaces and must not become opaque.
          (filter (lambda (path) (not (preparation-native-object? path library)))
            (preparation-files library
              (lambda (path)
                (not (or (equal? path namespace)
                         (string-prefix? (path-expand "static/gerbil-parser__" library) path))))))))
    (list (compiler-gsc-options)
          (map (lambda (name)
                 (cons name (if (equal? name "PATH")
                              (canonical-build-search-path (getenv name #f))
                              (getenv name #f))))
               '("GERBIL_GCC" "GERBIL_GSC" "GAMBOPT" "CC" "CFLAGS" "CPPFLAGS" "LDFLAGS" "SDKROOT"
                 "PATH" "LIBRARY_PATH" "MACOSX_DEPLOYMENT_TARGET" "GERBIL_BUILD_DEBUG"
                 "CORCXXFLAGS_GAMBUILD" "LDFLAGS_GAMBUILD"))
          (preparation-snapshot
           (append sources foreign
             (filter file-exists?
               (list (path-expand "bin/gsc" home) (path-expand "bin/gambuild-C" home)
                     (path-expand "include/gambit.h" home)
                     (getenv "GERBIL_GCC" "gcc"))))))))

(def (publication-record root)
  (let (path (path-expand "library-publication.sexp" root))
    (and (file-exists? path)
         (with-catch (lambda (_) #f)
           (lambda ()
             (let (record (call-with-input-file path read))
               (and (list? record) (= (length record) 3)
                    (eq? (car record) 'library-publication)
                    (list? (caddr record)) record)))))))

(def (call-with-library-reader root procedure)
  (let* ((root (publication-root root))
         (lock (open-output-file/lock (string-append root ".publication.lock") 0 op: LOCK_SH)))
    (try
      (let (record (publication-record root))
        (unless (and record (publication-inputs-current? root record)
                     (equal? (publication-products root) (caddr record)))
          (error "library has no complete admitted generation; rebuild the package"))
        (let (result (procedure))
          (unless (and (publication-inputs-current? root record)
                       (equal? (publication-products root) (caddr record)))
            (error "library inputs or products changed during validation"))
          result))
      (finally (device-close lock)))))

(def (publication-inputs-current? root record)
  ;; Re-enumerate the source closure: a newly added source invalidates admission
  ;; just as a modified or removed source does. Reader heap options may differ.
  (let (request (cadr record))
    (and (list? request) (= (length request) 2)
         (list? (cadr request)) (= (length (cadr request)) 3)
         (equal? (caddr (cadr request))
                 (caddr (library-publication-inputs root))))))

(def (withdraw-publication! root)
  (let (receipt (path-expand "library-publication.sexp" root))
    (when (file-exists? receipt) (delete-file receipt))))

(def (clean-library-products! root)
  (displayln "LIBRARY-CLEAN-START " root) (force-output)
  ;; std/make removes declared outputs. Its native loader variants and test
  ;; interfaces are not spec outputs; remove only this package's own namespace.
  (let* ((library (path-expand "lib" root))
         (namespace (path-expand "gerbil-parser" library))
         (static (path-expand "static" library)))
    (when (file-exists? namespace)
      (for-each
        (lambda (name)
          ;; Immutable grammar receipts are generator inputs, not loadable
          ;; module products. Their existing digest checks remain authoritative.
          (unless (member name '("compiled-language-artifacts"
                                 "compiled-language-parser-cache"
                                 "compiled-language-declaration-cache"
                                 "compiled-language-program-cache"))
            (delete-file-or-directory (path-expand name namespace) #t)))
        (directory-files namespace)))
    (when (file-exists? static)
      (for-each
        (lambda (name)
          (when (string-prefix? "gerbil-parser__" name)
            (delete-file-or-directory (path-expand name static) #t)))
        (directory-files static))))
  (displayln "LIBRARY-CLEAN-READY " root) (force-output))

(def (publication-products root)
  ;; Library packages do not own command binaries or source-only test products.
  (let ((library (path-expand "lib" root))
        (prefix (path-directory (path-expand "library-publication.sexp" root))))
    (map (lambda (path)
           (list (substring path (string-length prefix) (string-length path))
                 (preparation-file-digest path) (file-info-mode (file-info path))))
      (filter (lambda (path)
                (not (or (string-prefix? (path-expand "gerbil-parser/t/" library) path)
                         (string-prefix? (path-expand "static/gerbil-parser__t__" library) path)
                         (string-prefix? (path-expand "gerbil-parser/compiled-language-" library) path))))
              (append (preparation-files (path-expand "gerbil-parser" library))
                      (filter (lambda (path)
                                (and (string-prefix? (path-expand "static/gerbil-parser__" library) path)
                                     (string-suffix? ".scm" path)))
                              (preparation-files (path-expand "static" library))))))))

(def (call-with-library-publication root build (identity (lambda (directory)
                                      (list '("compile") (library-publication-inputs directory))))
                                    (reuse? #t) (clean clean-library-products!))
  (initialize-compiler-build!)
  (let* ((root (publication-root root))
         (_ (when (equal? root "/") (error "cannot publish filesystem root")))
         (lock (open-output-file/lock (string-append root ".publication.lock") 0)))
    (try
      (let* ((before (identity root)) (record (publication-record root))
             (current? (and record (equal? before (cadr record))
                            (equal? (publication-products root) (caddr record)))))
        (when (and (not reuse?) (not current?))
          (error "library generation is not current; build the package before extending it"))
        (if (and current? reuse?)
          (begin (displayln "LIBRARY-PUBLICATION-HIT") (force-output))
          (begin
            ;; Failure or process death leaves the generation unadmitted. There
            ;; is no second library tree, rollback copy, or destination swap.
            (withdraw-publication! root)
            (unless current? (clean root))
            (build root)
            (unless (equal? before (identity root))
              (error "library inputs changed during compilation"))
            (let* ((products (publication-products root))
                   (receipt (path-expand "library-publication.sexp" root))
                   (temporary (make-temporary-file-name (string-append receipt ".pending-"))))
              (unless (pair? products) (error "library compilation produced no modules"))
              (try
                (call-with-output-file temporary
                  (lambda (port) (write (list 'library-publication before products) port)))
                (rename-file temporary receipt #t)
                (finally (when (file-exists? temporary) (delete-file temporary)))))
            (displayln "LIBRARY-PUBLICATION-READY") (force-output))))
      (finally (device-close lock)))))

(def (publication-build-main compile-main (extension? #t))
  (lambda args
    (cond
      ((or (null? args) (equal? (car args) "compile"))
       (gc-report-set! #t)
       (call-with-library-publication (gerbil-path)
         (lambda (root)
           (call-with-compiler-import-progress (lambda () (apply compile-main args)))
           (prepare-library-loader-products! root))
         (lambda (root)
           (list (if (null? args) '("compile") args) (library-publication-inputs root)))
         (not extension?)
         (lambda (root)
           (apply compile-main '("clean"))
           (clean-library-products! root))))
      ((equal? (car args) "clean")
       ;; gerbil clean delegates to this standard std/build-script owner.
       ;; Withdraw admission BEFORE removing products, then run its actual
       ;; clean operation. An admission marker is never a substitute for clean.
       (let* ((root (publication-root (gerbil-path)))
              (receipt (path-expand "library-publication.sexp" root))
              (lock (open-output-file/lock (string-append root ".publication.lock") 0)))
         (try
           (withdraw-publication! root)
           (apply compile-main args)
           (displayln "LIBRARY-DECLARED-CLEAN-READY") (force-output)
           (unless extension? (clean-library-products! root))
           (finally (device-close lock)))))
      (else (apply compile-main args)))))
