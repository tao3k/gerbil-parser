;;; Shared content admission and package-product classification; no build entry.
(import (only-in :gerbil-parser/src/build-support/preparation-snapshot preparation-files preparation-file-digest preparation-snapshot preparation-resolve)
        (only-in :std/os/flock open-output-file/lock)
        (only-in :std/os/device device-close))

(export call-with-preparation-cache preparation-store-file conformance-source-only? conformance-module-product-matcher conformance-module-product? conformance-generated-cache?)

(def (conformance-source-only? name)
  (or (string-suffix? "-test" name)
      (member name '("t/fixtures/language-entries"
                     "t/generate-language-abi-alignment" "t/conformance-main"
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
  (let ((entries (caddr record)) (total (length (caddr record))) (index -1))
    (and (every (lambda (entry)
                  (set! index (+ index 1))
                  (let (stored (preparation-store-file store index))
                    (and (admit? (preparation-resolve (car entry))) (file-exists? stored)
                         (equal? (preparation-file-digest stored) (cadr entry))
                         (= (file-info-mode (file-info stored)) (caddr entry))))) entries)
         (begin
           (set! index -1)
           (for-each (lambda (entry)
                       (set! index (+ index 1))
                       (copy-preparation-product (preparation-store-file store index) (preparation-resolve (car entry)))
                       (when (or (zero? (modulo (+ index 1) 64))
                                 (= (+ index 1) total))
                         (displayln "PREPARATION-RESTORED files=" (+ index 1)
                                    " last=" (preparation-resolve (car entry))) (force-output))) entries)
           (displayln "CONFORMANCE-CACHE-RESTORED") (force-output)
           #t))))
(def (save-preparation-products! record store)
  (create-directory* store)
  (let ((receipt (path-expand "receipt.sexp" store))
        (total (length (caddr record))) (index -1))
    (when (file-exists? receipt) (delete-file receipt))
    (for-each (lambda (entry)
                (set! index (+ index 1))
                (copy-preparation-product (preparation-resolve (car entry)) (preparation-store-file store index))
                (when (or (zero? (modulo (+ index 1) 64))
                          (= (+ index 1) total))
                  (displayln "PREPARATION-STORED files=" (+ index 1)
                             " last=" (preparation-resolve (car entry))) (force-output)))
              (caddr record))
    (call-with-output-file receipt (lambda (port) (write record port)))
    (displayln "PREPARATION-STORED-RECEIPT files=" (+ index 1)) (force-output)))

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
               (displayln "PREPARATION-RECEIPT-READY files=" (length (caddr record)))
               (force-output)
               (when store (save-preparation-products! record store)))))))
     (finally (device-close owner))))))
