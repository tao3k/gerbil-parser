;;; Native implementation bodies remain compiled; closed loader programs use SCM.
(import (only-in "../modules/loader.ss" read-compiled-dependencies)
        (only-in "./preparation-snapshot.ss" preparation-files preparation-snapshot))
(export prepare-library-loader-products!)

(def (dynamic-loader-base path)
  (let (extension (path-extension path))
    (and (> (string-length extension) 2) (string-prefix? ".o" extension)
         (andmap char-numeric? (string->list (substring extension 2 (string-length extension))))
         (path-strip-extension path))))

(def (prepare-library-loader-products! root)
  ;; The publication writer owns source/product identity and the lock. Never
  ;; traverse SDK/foreign libraries, implementation sidecars, or raw executables.
  (let* ((namespace (path-expand "lib/gerbil-parser" root))
         (files (if (file-exists? namespace) (preparation-files namespace) '()))
         (wrappers
          (filter (lambda (path)
                    (and (string-suffix? ".scm" path)
                         (not (string-contains (path-strip-directory path) "~"))
                         (file-exists? (string-append (path-strip-extension path) ".ssi"))
                         (read-compiled-dependencies path #t))) files))
         (wrapper-index (make-table test: equal?))
         (_ (for-each (lambda (source) (table-set! wrapper-index (path-strip-extension source) #t)) wrappers))
         (bootstrap
          (map (lambda (name) (path-expand name namespace))
               '("src/modules/loader" "t/support/qualification/process-child")))
         (objects
          (filter (lambda (path)
                    ;; The shared loader must bootstrap before source replay;
                    ;; the child launcher is an explicitly executed native image.
                    (let (base (dynamic-loader-base path))
                      (and base (not (member base bootstrap))
                           (table-ref wrapper-index base #f)))) files))
         (retained (filter (lambda (path) (not (member path objects))) files))
         (before (preparation-snapshot retained)))
    (for-each delete-file objects)
    (unless (equal? before (preparation-snapshot retained))
      (error "loader preparation changed retained library products"))
    (displayln "LIBRARY-LOADER-PRODUCTS source-wrappers=" (length objects))
    (force-output)))
