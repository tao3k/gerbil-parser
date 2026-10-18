;;; Thin conformance preparation owner; reusable cache logic lives in the engine.
(import (only-in :gerbil-parser/src/build-support/preparation-cache call-with-preparation-cache preparation-store-file conformance-source-only? conformance-module-product-matcher conformance-module-product? conformance-generated-cache?)
        (only-in :gerbil-parser/src/build-support/preparation-snapshot preparation-files preparation-native-object?))

(def (conformance-owned-modules)
  ;; Read the build owner's literal catalog; do not duplicate its module list.
  (call-with-input-file "build-language-conformance.ss"
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (cond ((eof-object? form) (error "missing conformance module catalog"))
                ((and (pair? form) (eq? (car form) 'def)
                      (equal? (cadr form) 'conformance-modules))
                 (cadr (caddr form)))
                (else (loop))))))))
(def (prepare-conformance-content!)
  (let* ((library (path-expand "lib" (getenv "GERBIL_PATH")))
         (binary (path-expand "bin/gerbil-parser-conformance" (getenv "GERBIL_PATH")))
         (home (getenv "GERBIL_BUILD_PREFIX" (gerbil-home)))
         (owned (conformance-owned-modules))
         (helpers (filter (lambda (name) (not (conformance-source-only? name))) owned))
         (owned-product? (conformance-module-product-matcher library owned))
         (helper-product? (conformance-module-product-matcher library helpers)))
    (def (inputs)
      ;; Conservative closure: all project sources and dependency products,
      ;; including macro interfaces, plus the actual SDK and compiler identity.
      (append
        (apply append (map preparation-files '("src" "languages" "t" "include")))
        (filter (lambda (path) (string-suffix? ".ss" path))
                (preparation-files "scripts"))
        (filter (lambda (path) (string-suffix? ".ss" path))
                (list-sort string<? (directory-files ".")))
        '("gerbil.pkg")
        (filter (lambda (path)
                  (not (or (owned-product? path)
                           (conformance-generated-cache? path library)
                           (preparation-native-object? path library))))
                (preparation-files library))
        (preparation-files (path-expand "pkg" (getenv "GERBIL_PATH")))
        (preparation-files (path-expand "include" home))
        (preparation-files (path-expand "lib" home))
        (preparation-files (path-expand "bin" home))))
    ;; GCC version/options and environment are executable preparation inputs.
    ;; Capture them as a file so the same content admission covers this identity.
    (let (toolchain (path-expand "conformance-toolchain.sexp" (getenv "GERBIL_PATH")))
      (when (file-exists? toolchain) (delete-file toolchain))
      (call-with-output-file toolchain
        (lambda (port)
          (write (map (lambda (name) (cons name (getenv name #f)))
                      '("GERBIL_GCC" "GERBIL_GSC" "GERBIL_BUILD_PREFIX" "CC" "CFLAGS" "LDFLAGS"
                        "MACOSX_DEPLOYMENT_TARGET" "GERBIL_LOADPATH" "LIBRARY_PATH" "CPPFLAGS" "SDKROOT" "PATH")) port)
          (let* ((compiler (getenv "GERBIL_GCC" "gcc"))
                 (path (if (string-contains compiler "/") compiler
                         (find file-exists?
                           (map (lambda (root) (path-expand compiler root))
                                (string-split (getenv "PATH") #\:))))))
            (unless path (error "C compiler identity is missing" compiler))
            (write (list path (preparation-file-digest path)) port))))
      (call-with-preparation-cache
        (path-expand "conformance-cache.sexp" (getenv "GERBIL_PATH"))
        (lambda () (cons toolchain (inputs)))
        (lambda ()
          (if (not (file-exists? binary)) '()
            (cons binary
              (filter (lambda (path)
                        (and (not (string-contains path "/static/"))
                             (helper-product? path)))
                      (preparation-files library)))))
        (lambda ()
          (call-with-compiled-interface-trace
            (lambda ()
              (load "build-language-conformance.ss")
              (eval '(compile-static-conformance!))
              (load "build-language-conformance-link.ss")
              (eval '(main)))))
        (path-expand "conformance-store" (getenv "GERBIL_PATH"))
        (lambda (path)
          (and (equal? path (path-normalize path))
               (or (equal? path binary)
                   (and (not (string-contains path "/static/"))
                        (helper-product? path)))))))))

;; The complete native helper closure is published through the same library
;; generation barrier as the package. Failure withdraws admission; partial
;; products remain unadmitted until a successful preparation publishes them.
(import (only-in :gerbil-parser/src/build-support/library-publication
                 call-with-library-publication library-publication-inputs))
(def (prepare-conformance!)
  (call-with-library-publication (gerbil-path)
    (lambda (_stage) (prepare-conformance-content!))
    (lambda (root) (list '("compile") (library-publication-inputs root))) #f))
