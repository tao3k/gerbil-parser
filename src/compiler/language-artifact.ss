;;; -*- Gerbil -*-
;;; Build-time storage boundary for materialized language IR.

(import (only-in :std/list/list delete-duplicates/hash)
        (only-in ../runtime/identity sha256-text)
        (only-in ../runtime/language-artifact
                 compiled-language-artifact-relative-path
                 load-compiled-language-artifact/roots
                 sha256-identity-filename)
        (only-in :gerbil/compiler/base current-compile-output-dir)
        (only-in :std/misc/ports read-all-as-u8vector)
        (only-in :std/encoding/base64 base64-encode)
        (only-in :std/string/utf8 utf8->string)
        (only-in :std/encoding/zlib compress uncompress))
(export project-language-catalog materialize-compiled-language-artifact
        materialize-compiled-language-artifact/output-dirs
        compile-language-parser-artifact
        compile-language-parser-artifact/output-dirs
        compile-language-declaration-artifacts
        compile-language-declaration-artifacts/output-dirs
        encode-compiled-language-artifact)

(def +parser-artifact-cache-schema+
  "gerbil-parser.parser-artifact-cache.v1")
(def +parser-artifact-generator-contract+
  "gerbil-parser.lalr1-generator.v2")
(def +language-declaration-cache-schema+
  "gerbil-parser.language-declaration-cache.v1")
(def +language-declaration-generator-contract+
  "gerbil-parser.language-declaration-generator.v2")

;; v2 publishes layout-guard actions. Older declaration/parser receipts must
;; regenerate even when their authored grammar bytes have not changed.

;; : (-> Datum String)
(def (serialize value)
  (call-with-output-string (lambda (port) (write value port))))

;; : (-> String String (-> InputPort String) Boolean)
(def (content-matches? path serialized read-content)
  (and (file-exists? path)
       (with-exception-catcher
        (lambda (_) #f)
        (lambda ()
          (equal? serialized
                  (call-with-input-file path read-content))))))

(def (materialized-content-matches? path serialized)
  (content-matches?
   path serialized
   (lambda (port)
     (utf8->string (uncompress (read-all-as-u8vector port))))))

;; Both sidecars and cache receipts are immutable. A competing writer is
;; accepted only after its complete content has been independently verified.
(def (publish-immutable-content! path serialized matches? write-content!
                                 conflict-message temporary-message)
  (if (file-exists? path)
    (unless (matches? path serialized)
      (error conflict-message path))
    (let make-temporary ()
      (let (temporary
            (string-append path ".tmp."
                           (number->string (random-integer 1073741824))))
        (if (file-exists? temporary)
          (make-temporary)
          (with-exception-catcher
           (lambda (exception)
             (when (file-exists? temporary)
               (delete-file temporary))
             (if (matches? path serialized)
               (void)
               (raise exception)))
           (lambda ()
             (call-with-output-file temporary write-content!)
             (unless (matches? temporary serialized)
               (error temporary-message temporary))
             (rename-file temporary path #f))))))))

;; : (-> String U8Vector String Void)
(def (publish-materialized-content! path bytes serialized)
  (publish-immutable-content!
   path serialized materialized-content-matches?
   (lambda (port)
     (write-subu8vector bytes 0 (u8vector-length bytes) port))
   "compiled language artifact target contains different bytes"
   "compiled language artifact temporary write is invalid"))

;; : (forall (a) (-> a [String] [String]))
;; : (-> Datum List List)
(def (materialize-compiled-language-artifact/output-dirs value output-dirs)
  (let* ((serialized (serialize value))
         (digest (sha256-text serialized))
         (relative-path
          (compiled-language-artifact-relative-path digest))
         (paths (map (lambda (output-dir)
                       (path-expand relative-path output-dir))
                     output-dirs))
         (missing (filter (lambda (path) (not (file-exists? path))) paths)))
    ;; Compression level 9 is intentionally paid only when publishing new
    ;; content. Warm expansion validates existing immutable sidecars without
    ;; recompressing multi-megabyte Grammar/LR datums.
    (for-each
     (lambda (path)
       (when (file-exists? path)
         (unless (materialized-content-matches? path serialized)
           (error "compiled language artifact target contains different bytes"
                  path))))
     paths)
    (unless (null? missing)
      (let (bytes (compress (string->utf8 serialized) compression: 9))
        (for-each
         (lambda (path)
           (create-directory* (path-directory path))
           (publish-materialized-content! path bytes serialized))
         missing)))
    (list relative-path digest)))

;; : (-> String String Boolean)
(def (text-content-matches? path serialized)
  (content-matches?
   path serialized
   (lambda (port) (read-line port #f))))

;; : (-> String String Void)
(def (publish-text-content! path serialized)
  (publish-immutable-content!
   path serialized text-content-matches?
   (lambda (port) (display serialized port))
   "compiled language cache receipt contains different bytes"
   "compiled language cache temporary write is invalid"))

;; : (-> String String)
(def (parser-cache-relative-path key)
  (string-append "gerbil-parser/compiled-language-parser-cache/"
                 (sha256-identity-filename key) ".scm"))

(def (cache-receipt-field receipt field)
  (let (row (assq field receipt))
    (and row (cdr row))))

;; A receipt must contain exactly one datum with the expected identity and
;; all fields needed by its caller. Cache kinds differ only in their schema
;; and required locator fields.
(def (read-cache-receipt relative-path output-dirs schema key required-fields
                         invalid-message)
  (let (path (find file-exists?
                   (map (lambda (root) (path-expand relative-path root))
                        output-dirs)))
    (and path
         (call-with-input-file
          path
          (lambda (port)
            (let ((receipt (read port)) (trailing (read port)))
              (unless (and (eof-object? trailing)
                           (list? receipt)
                           (equal? (cache-receipt-field receipt 'schema)
                                   schema)
                           (equal? (cache-receipt-field receipt 'key) key)
                           (andmap (lambda (field) (assq field receipt))
                                   required-fields))
                (error invalid-message path receipt))
              receipt))))))

;; : (-> String List (Maybe List))
(def (read-parser-cache-receipt key output-dirs)
  (read-cache-receipt
   (parser-cache-relative-path key) output-dirs
   +parser-artifact-cache-schema+ key '(artifact)
   "invalid compiled language parser cache receipt"))

;; : (-> Alist Alist)
(def (parser-artifact-with-materialization value)
  (if (assq 'materialization value)
    value
    (cons (car value)
          (cons '(materialization . aot-expansion) (cdr value)))))

;;; Expensive LR generation is keyed before it runs. A fresh expander import
;;; validates and loads the immutable sidecar instead of rebuilding the same
;;; automaton during std/make's dependency-planning phase.
;; : (-> Datum (-> Datum) [String] (values Datum List Symbol))
(def (compile-language-parser-artifact/output-dirs grammar compile output-dirs)
  (let* ((key
          (sha256-text
           (serialize
            (list +parser-artifact-generator-contract+ grammar))))
         (receipt (read-parser-cache-receipt key output-dirs)))
    (if receipt
      (let (locator (cache-receipt-field receipt 'artifact))
        (values
         (load-compiled-language-artifact/roots
          "gerbil-parser.parser-ir.v1" locator output-dirs)
         locator
         'hit))
      (begin
        (display "... generate parser artifact ")
        (displayln (let (row (assq 'grammar grammar))
                     (if row (cdr row) key)))
        (force-output)
        (let* ((value (parser-artifact-with-materialization (compile)))
               (locator
                (materialize-compiled-language-artifact/output-dirs
                 value output-dirs))
               (cache-receipt
                `((schema . ,+parser-artifact-cache-schema+)
                  (key . ,key)
                  (generatorContract . ,+parser-artifact-generator-contract+)
                  (artifact . ,locator)))
               (serialized (serialize cache-receipt))
               (relative-path (parser-cache-relative-path key)))
          (for-each
           (lambda (root)
             (let (path (path-expand relative-path root))
               (create-directory* (path-directory path))
               (publish-text-content! path serialized)))
           output-dirs)
          (values value locator 'miss))))))

;; : (-> [String])
(def (current-artifact-output-dirs)
  (let* ((compile-output-dir (current-compile-output-dir))
         (canonical-output-dir (path-expand "lib" (gerbil-path)))
         (output-dirs
          (if (and (string? compile-output-dir)
                   (not (equal? compile-output-dir "."))
                   (not (equal? (path-expand compile-output-dir)
                                canonical-output-dir)))
            (list (path-expand compile-output-dir) canonical-output-dir)
            (list canonical-output-dir))))
    output-dirs))

;;; Encodes the already compressed immutable artifact for a linked AOT image.
;;; The sidecar remains the build cache; only its compressed bytes cross into
;;; the runtime module, so the uncompressed Grammar/LR datum is never emitted
;;; as per-character generated C.
;; : (-> List String)
(def (encode-compiled-language-artifact locator)
  (let* ((relative-path (car locator))
         (path
          (or (find file-exists?
                    (map (lambda (root) (path-expand relative-path root))
                         (current-artifact-output-dirs)))
              (error "compiled language artifact sidecar is unavailable"
                     relative-path))))
    (base64-encode
     (call-with-input-file path read-all-as-u8vector))))

;; : (-> Datum (-> Datum) (values Datum List Symbol))
(def (compile-language-parser-artifact grammar compile)
  (compile-language-parser-artifact/output-dirs
   grammar compile (current-artifact-output-dirs)))

;; : (-> String String)
(def (declaration-cache-relative-path key)
  (string-append "gerbil-parser/compiled-language-declaration-cache/"
                 (sha256-identity-filename key) ".scm"))

;; : (-> String List (Maybe List))
(def (read-declaration-cache-receipt key output-dirs)
  (read-cache-receipt
   (declaration-cache-relative-path key) output-dirs
   +language-declaration-cache-schema+ key '(grammar bound parser)
   "invalid compiled language declaration cache receipt"))

;;; Caches the complete declaration projection, not just the final parser IR.
;;; On a hit expansion receives receipt-bound content-addressed locators and avoids
;;; rebinding, serializing, compressing, or loading large derived datums.
;; : (-> Datum Datum (-> Datum) (-> Datum) [String]
;;        (values List List List Symbol))
(def (compile-language-declaration-artifacts/output-dirs
      declaration-identity grammar compile-bound compile-parser output-dirs)
  (let* ((key
          (sha256-text
           (serialize
            (list +language-declaration-generator-contract+
                  declaration-identity grammar))))
         (receipt (read-declaration-cache-receipt key output-dirs)))
    (if receipt
      (values (cache-receipt-field receipt 'grammar)
              (cache-receipt-field receipt 'bound)
              (cache-receipt-field receipt 'parser)
              'hit)
      (let* ((grammar-locator
              (materialize-compiled-language-artifact/output-dirs
               grammar output-dirs))
             (bound-locator
              (materialize-compiled-language-artifact/output-dirs
               (compile-bound) output-dirs))
             (parser-locator
              (let-values (((_value locator _status)
                            (compile-language-parser-artifact/output-dirs
                             grammar compile-parser output-dirs)))
                locator))
             (cache-receipt
              `((schema . ,+language-declaration-cache-schema+)
                (key . ,key)
                (generatorContract
                 . ,+language-declaration-generator-contract+)
                (grammar . ,grammar-locator)
                (bound . ,bound-locator)
                (parser . ,parser-locator)))
             (serialized (serialize cache-receipt))
             (relative-path (declaration-cache-relative-path key)))
        (for-each
         (lambda (output-dir)
           (let (path (path-expand relative-path output-dir))
             (create-directory* (path-directory path))
             (publish-text-content! path serialized)))
         output-dirs)
        (values grammar-locator bound-locator parser-locator 'miss)))))

;; : (-> Datum Datum (-> Datum) (-> Datum) (values List List List Symbol))
(def (compile-language-declaration-artifacts declaration-identity grammar
                                             compile-bound compile-parser)
  (compile-language-declaration-artifacts/output-dirs
   declaration-identity grammar compile-bound compile-parser
   (current-artifact-output-dirs)))

;;; Persisting the compressed payload outside the generated module prevents
;;; Gambit from expanding large immutable IR into per-character C initializers.
;; : (forall (a) (-> a [String]))
;; : (-> Datum List)
(def (materialize-compiled-language-artifact value)
  (materialize-compiled-language-artifact/output-dirs
   value (current-artifact-output-dirs)))

;;; Validate a published ABI projection over already inferred concise syntax.
;;; Unused token kinds require an explicit reservation rather than lexer remaps.
(def (project-language-catalog inferred-kinds inferred-terminals catalog)
  (def (require condition message) (unless condition (error message)))
  (def (unique rows) (delete-duplicates/hash rows))
  (if (not catalog)
    (values inferred-kinds inferred-terminals)
    (begin
      (require (and (list? catalog) (memv (length catalog) '(3 4))
                    (every (lambda (row) (and (list? row) (pair? row))) (cdr catalog))
                    (equal? (map car (cdr catalog))
                            (if (= (length catalog) 3) '(syntax-kinds terminals)
                                '(syntax-kinds terminals reserved-token-kinds))))
               "catalog requires syntax-kinds and terminals sections")
      (let ((kinds (cdadr catalog)) (terminals (cdaddr catalog))
            (reserved (if (= (length catalog) 4) (cdr (list-ref catalog 3)) '())))
        (require (every (lambda (row)
                          (and (list? row) (= (length row) 3)
                               (symbol? (car row)) (memq (cadr row) '(node token))
                               (list? (caddr row)) (every symbol? (caddr row))
                               (= (length (caddr row)) (length (unique (caddr row)))))) kinds)
                 "catalog has an invalid syntax-kind row")
        (require (and (pair? kinds) (eq? (cadar kinds) 'node))
                 "catalog must start with a node kind")
        (require (= (length kinds) (length (unique (map car kinds))))
                 "catalog has duplicate syntax kinds")
        (for-each
         (lambda (inferred)
           (let (published (assq (car inferred) kinds))
             (require (and published (eq? (cadr published) (cadr inferred))
                           (every (lambda (field) (memq field (caddr published))) (caddr inferred)))
                      "catalog must retain every inferred kind, category and field"))) inferred-kinds)
        (require (and (every symbol? reserved) (= (length reserved) (length (unique reserved)))
                      (every (lambda (name)
                               (let (row (assq name kinds))
                                 (and row (eq? (cadr row) 'token)
                                      (not (assq name inferred-kinds))))) reserved))
                 "catalog reservations must name unique unused token kinds")
        (require (every (lambda (row)
                          (or (assq (car row) inferred-kinds) (eq? (cadr row) 'node)
                              (memq (car row) reserved))) kinds)
                 "catalog may reserve nodes but cannot invent token kinds")
        (require (every (lambda (row)
                          (and (list? row) (= (length row) 2) (every symbol? row))) terminals)
                 "catalog has an invalid terminal row")
        (require (and (= (length terminals) (length inferred-terminals))
                      (= (length terminals) (length (unique (map car terminals))))
                      (every (lambda (row) (equal? (assq (car row) terminals) row)) inferred-terminals))
                 "catalog must retain the exact inferred terminal mapping")
        (values kinds terminals)))))
