;;; Content admission for the complete compiled conformance preparation owner.
(import (only-in :std/crypto/digest Digest::sha256 digest-update! digest-final!)
        (only-in :std/os/flock open-output-file/lock)
        (only-in :std/os/device device-close))

(def (preparation-files root)
  ;; Preserve sorted depth-first identity with one accumulator. Report actual
  ;; discovered files, including the post-link admission walk.
  (let ((files '()) (count 0))
    (def (walk path)
      (when (file-exists? path)
        (if (eq? (file-type path) 'directory)
          (for-each (lambda (name)
                      (unless (string-prefix? "." name)
                        (walk (path-expand name path))))
                    (list-sort string<? (directory-files path)))
          (begin
            (set! files (cons path files))
            (set! count (+ count 1))
            (when (zero? (modulo count 128))
              (displayln "PREPARATION-DISCOVERED files=" count " last=" path)
              (force-output))))))
    (walk root)
    (displayln "PREPARATION-DISCOVERED root=" root " files=" count) (force-output)
    (reverse files)))
(def (preparation-file-digest path)
  (let ((digest (Digest::sha256)) (buffer (make-u8vector 65536)))
    (call-with-input-file path
      (lambda (port)
        (let loop ()
          (let (count (read-subu8vector buffer 0 (u8vector-length buffer) port))
            (unless (zero? count)
              (digest-update! digest buffer 0 count)
              (loop))))))
    (digest-final! digest)))
(def (preparation-snapshot files)
  (let ((count 0) (total (length files)))
    (displayln "PREPARATION-INVENTORY files=" total) (force-output)
    (map (lambda (path)
           (let (digest (preparation-file-digest path))
             (set! count (+ count 1))
             (when (or (zero? (modulo count 64)) (= count total))
               (displayln "PREPARATION-HASHED files=" count " last=" path) (force-output))
             (list path digest (file-info-mode (file-info path))))) files)))

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
(def (conformance-source-only? name)
  (or (string-suffix? "-test" name)
      (member name '("t/generate-language-abi-alignment" "t/conformance-main"
                     "t/benchmarks/versioned-languages/all-languages"))))
(def (conformance-module-product-matcher library names)
  ;; Module topology is invariant across a complete inventory. Expand paths
  ;; once, rather than once per file and per candidate module.
  (let (prefixes
         (apply append
           (map (lambda (name)
                  (let ((prefix (path-expand (string-append "gerbil-parser/" name) library))
                        (static-prefix
                          (path-expand (string-append "gerbil-parser__"
                            (string-join (string-split name #\/) "__"))
                            (path-expand "static" library))))
                    (list (string-append prefix ".") (string-append prefix "~")
                          (string-append static-prefix ".")
                          (string-append static-prefix "~")))) names)))
    (lambda (path)
      (any (lambda (prefix) (string-prefix? prefix path)) prefixes))))
(def (conformance-module-product? path library names)
  ((conformance-module-product-matcher library names) path))
(def (conformance-generated-cache? path library)
  ;; Content-addressed compiler memo products are created during preparation.
  ;; Their source and compiler inputs remain in the preparation snapshot.
  (any (lambda (name)
         (string-prefix?
           (string-append (path-expand (string-append "gerbil-parser/" name) library) "/")
           path))
       '("compiled-language-artifacts" "compiled-language-parser-cache"
         "compiled-language-declaration-cache" "compiled-language-program-cache")))
(def (conformance-generated-object? path library)
  ;; Native object option records are successful build products, not dependencies.
  ;; Source/GSC/options owners remain inputs; contain this exclusion in static/.
  (and (string-prefix? (string-append (path-expand "static" library) "/") path)
       (or (string-suffix? ".c" path) (string-suffix? ".o" path)
           (string-suffix? ".o.options.sexp" path))))
(def (preparation-record path)
  (and (file-exists? path)
       (with-catch (lambda (_) #f)
         (lambda ()
           (let (record (call-with-input-file path read))
             (and (list? record) (= (length record) 3)
                  (eq? (car record) 'conformance-preparation-v1)
                  (list? (cadr record)) (pair? (caddr record)) (list? (caddr record))
                  (every (lambda (entry)
                           (and (list? entry) (= (length entry) 3)
                                (string? (car entry)) (u8vector? (cadr entry))
                                (= 32 (u8vector-length (cadr entry))) (integer? (caddr entry))))
                         (caddr record))
                  record))))))
(def (preparation-store-file store index)
  (path-expand (string-append "product-" (number->string index)) store))
(def (copy-preparation-product from to)
  (create-directory* (path-directory to))
  (when (file-exists? to) (delete-file to))
  ;; Gambit's copy-file creates a new mode; preserve executable/read permissions
  ;; explicitly because they are part of the admitted product identity.
  (call-with-input-file from
    (lambda (input)
      (call-with-output-file (list path: to permissions: (file-mode from))
        (lambda (output)
          (let (buffer (make-u8vector 65536))
            (let loop ()
              (let (count (read-subu8vector buffer 0 (u8vector-length buffer) input))
                (unless (zero? count)
                  (write-subu8vector buffer 0 count output)
                  (loop))))))))))
(def (restore-preparation-products! record store admit?)
  (let ((entries (caddr record)) (index -1))
    (and (every (lambda (entry)
                  (set! index (+ index 1))
                  (let (stored (preparation-store-file store index))
                    (and (admit? (car entry)) (file-exists? stored)
                         (equal? (preparation-file-digest stored) (cadr entry))
                         (= (file-info-mode (file-info stored)) (caddr entry))))) entries)
         (begin
           (set! index -1)
           (for-each (lambda (entry)
                       (set! index (+ index 1))
                       (copy-preparation-product (preparation-store-file store index) (car entry))) entries)
           (displayln "CONFORMANCE-CACHE-RESTORED") (force-output)
           #t))))
(def (save-preparation-products! record store)
  (create-directory* store)
  (let ((receipt (path-expand "receipt.sexp" store)) (index -1))
    (when (file-exists? receipt) (delete-file receipt))
    (for-each (lambda (entry)
                (set! index (+ index 1))
                (copy-preparation-product (car entry) (preparation-store-file store index)))
              (caddr record))
    (call-with-output-file receipt (lambda (port) (write record port)))))

(def (call-with-preparation-cache receipt inputs outputs prepare
                                  (store #f) (admit? (lambda (_) #f)))
  ;; The caller owns all writes. Reject simultaneous owners rather than admitting
  ;; a receipt while another compiler can mutate the same products.
  (let (lock (string-append receipt ".lock"))
    (create-directory* (path-directory receipt))
    (let (owner (open-output-file/lock lock 0))
    (try
     (let* ((identity (preparation-snapshot (inputs)))
            (local (preparation-record receipt))
            (saved (or local (and store (preparation-record (path-expand "receipt.sexp" store)))))
            (_ (when (and (not local) saved store (equal? identity (cadr saved)))
                 (unless (restore-preparation-products! saved store admit?) (set! saved #f))))
            (products (outputs)))
       (if (and saved
                (equal? identity (cadr saved))
                (pair? products)
                (equal? (preparation-snapshot products) (caddr saved)))
         (begin
           (unless local
             (call-with-output-file receipt (lambda (port) (write saved port))))
           (displayln "CONFORMANCE-CACHE-HIT") (force-output))
         (begin
           ;; Failed rebuilds must not leave a usable old certificate.
           (when (file-exists? receipt) (delete-file receipt))
           (displayln "CONFORMANCE-CACHE-MISS") (force-output)
           (prepare)
           (unless (equal? identity (preparation-snapshot (inputs)))
             (error "conformance preparation inputs changed during build"))
           (let (products (outputs))
             (unless (pair? products) (error "conformance preparation produced no artifacts"))
             (let (record (list 'conformance-preparation-v1 identity
                               (preparation-snapshot products)))
               (call-with-output-file receipt (lambda (port) (write record port)))
               (when store (save-preparation-products! record store)))))))
     (finally (device-close owner))))))

(def (prepare-conformance!)
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
                           (conformance-generated-object? path library))))
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
