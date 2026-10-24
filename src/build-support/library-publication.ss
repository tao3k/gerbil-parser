;;; std/make owns in-place compilation and cleaning. Readers admit only a
;;; completed, source-bound generation under the same reader/writer lock.
(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/os/flock open-output-file/lock LOCK_SH)
        (only-in :std/os/device device-close)
        "./preparation-snapshot.ss" "./compiler-profile.ss"
        "./build-environment.ss"
        "./package-inputs.ss")
(export call-with-library-publication library-publication-inputs publication-build-main call-with-library-reader)

(def (publication-root path)
  (let loop ((path (path-normalize (path-expand path))))
    (if (and (> (string-length path) 1) (string-suffix? "/" path))
      (loop (substring path 0 (- (string-length path) 1))) path)))

(def (library-publication-inputs root (owner #f))
  (when (and owner (not (equal? owner (current-package-input-owner))))
    (error "publication input policy belongs to another package source" owner))
  (let* ((library (path-expand "lib" root))
         (policy (and owner (package-input-policy owner root)))
         (home (gerbil-home))
         (sources
          (append (apply append (map preparation-files '("src" "modules" "languages" "language-support" "include")))
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
                (and (or (not policy) (package-input-path? policy path))
                     (not (or (equal? path namespace)
                              (string-prefix? (path-expand "static/gerbil-parser__" library) path)))))))))
    (append (list (compiler-gsc-options)
          (map (lambda (name)
                 (cons name (cond
                              ((equal? name "PATH") (canonical-build-search-path (getenv name #f)))
                              ((equal? name "GAMBOPT") (canonical-build-runtime-options (getenv name #f)))
                              (else (getenv name #f)))))
               '("GERBIL_GCC" "GERBIL_GSC" "GAMBOPT" "CC" "CFLAGS" "CPPFLAGS" "LDFLAGS" "SDKROOT"
                 "PATH" "COMPILER_PATH" "LIBRARY_PATH" "MACOSX_DEPLOYMENT_TARGET" "GERBIL_BUILD_DEBUG"
                 "CORCXXFLAGS_GAMBUILD" "LDFLAGS_GAMBUILD"))
          (preparation-snapshot
           (append sources foreign (if policy (package-input-metadata policy) '())
             (filter file-exists?
               (list (path-expand "bin/gsc" home) (path-expand "bin/gambuild-C" home)
                     (path-expand "include/gambit.h" home)
                     (getenv "GERBIL_GCC" "gcc"))))))
      (if owner (list (list 'package-build owner)) '()))))

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
         (list? (cadr request))
         (let* ((inputs (cadr request))
                (scope (and (= (length inputs) 4) (list-ref inputs 3)))
                (owner (and (list? scope) (= (length scope) 2)
                            (eq? (car scope) 'package-build) (string? (cadr scope))
                            (cadr scope))))
           (and (or (= (length inputs) 3) owner)
                (let (current (library-publication-inputs root owner))
                  (and (equal? (caddr inputs) (caddr current))
                       (equal? (cdddr inputs) (cdddr current)))))))))

;; A whole-environment extension may consume a current package-scoped base.
;; Compiler options/environment, source support and products remain mandatory.
(def (base-extension-current? root record before)
  (with-catch (lambda (_invalid) #f)
    (lambda ()
      (let* ((request (cadr record)) (inputs (cadr request))
             (next (cadr before)) (scope (list-ref inputs 3)))
        (and (list? request) (= (length request) 2)
             (= (length inputs) 4) (list? scope) (= (length scope) 2)
             (eq? (car scope) 'package-build)
             (list? before) (= (length before) 2) (list? next) (= (length next) 3)
             (equal? (car request) (car before))
             (equal? (car inputs) (car next))
             (equal? (cadr inputs) (cadr next))
             (publication-inputs-current? root record))))))

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

(def (report-publication-row-drift component expected current)
  (let loop ((expected expected) (current current))
    (cond
     ((equal? expected current) (void))
     ((and (pair? expected) (pair? current) (equal? (car expected) (car current)))
      (loop (cdr expected) (cdr current)))
     (else
      (displayln "LIBRARY-PUBLICATION-DRIFT component=" component
                 " expected=" (and (pair? expected) (caar expected))
                 " current=" (and (pair? current) (caar current))
                 " expected-mode=" (and (pair? expected) (caddar expected))
                 " current-mode=" (and (pair? current) (caddar current)))))))

(def (report-publication-drift record before products)
  (with-catch
   (lambda (_) (displayln "LIBRARY-PUBLICATION-DRIFT component=invalid-record"))
   (lambda ()
     (if (not record)
       (displayln "LIBRARY-PUBLICATION-DRIFT component=missing-record")
       (let ((expected (cadr (cadr record))) (current (cadr before)))
         (unless (equal? (car expected) (car current))
           (displayln "LIBRARY-PUBLICATION-DRIFT component=compiler-options"))
         (for-each
          (lambda (entry)
            (unless (equal? entry (assoc (car entry) (cadr current)))
              (displayln "LIBRARY-PUBLICATION-DRIFT environment=" (car entry))))
          (cadr expected))
         (report-publication-row-drift 'products (caddr record) products)
         (report-publication-row-drift 'requested-source (caddr expected) (caddr current)))))))

(def (call-with-library-publication root build (identity (lambda (directory)
                                      (list '("compile") (library-publication-inputs directory))))
                                    (reuse? #t) (clean clean-library-products!))
  (initialize-compiler-build!)
  (let* ((root (publication-root root))
         (_ (when (equal? root "/") (error "cannot publish filesystem root")))
         (lock (open-output-file/lock (string-append root ".publication.lock") 0)))
    (try
      (let* ((before (identity root)) (record (publication-record root))
             (products (and record (publication-products root)))
             (products-current? (and record (equal? products (caddr record))))
             (current? (and products-current? (equal? before (cadr record))))
             (extension? (and (not reuse?) (not current?) products-current?
                              (base-extension-current? root record before))))
        (when (and (not reuse?) (not (or current? extension?)))
          (report-publication-drift record before products)
          (error "library generation is not current; build the package before extending it"))
        (if (and current? reuse?)
          (begin (displayln "LIBRARY-PUBLICATION-HIT") (force-output))
          (begin
            ;; Failure or process death leaves the generation unadmitted. There
            ;; is no second library tree, rollback copy, or destination swap.
            (withdraw-publication! root)
            (unless (or current? extension?) (clean root))
            (build root)
            (unless (equal? before (identity root))
              (error "library inputs changed during compilation"))
            (when (and extension? (not (publication-inputs-current? root record)))
              (error "base package inputs changed during extension"))
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
       (let* ((owner (and (not extension?) (current-package-input-owner)))
              (policy (and owner (package-input-policy owner (gerbil-path)))))
        (call-with-library-publication (gerbil-path)
         (lambda (root)
           ;; Core/SDK products remain intact after their completeness check.
           ;; Deleting native loaders here would immediately invalidate the
           ;; producer's result and force every later repair to rebuild them.
           (call-with-compiler-import-progress (lambda () (apply compile-main args))
             (if policy (lambda (path) (package-input-admit-import! policy path)) void)))
         (lambda (root)
           (list (if (null? args) '("compile") args) (library-publication-inputs root owner)))
         (not extension?)
         (lambda (root)
           (apply compile-main '("clean"))
           (clean-library-products! root)))))
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
