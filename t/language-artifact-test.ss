#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Independent positive, negative, and concurrent admission for AOT IR files.

(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/sync/barrier
                 barrier-post! barrier-wait! make-barrier)
        (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/base64 base64-encode)
        (only-in :std/encoding/zlib compress)
        (only-in :gerbil-parser/src/compiler/language-artifact
                 compile-language-declaration-artifacts/output-dirs
                 compile-language-parser-artifact/output-dirs
                 materialize-compiled-language-artifact/output-dirs)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :gerbil-parser/src/runtime/language-artifact
                 compiled-language-artifact-relative-path
                 load-compiled-language-artifact/embedded
                 load-compiled-language-artifact/roots
                 sha256-identity-filename))
(export language-artifact-tests)

(def test-schema "gerbil-parser.language-artifact-test.v1")
(def test-value
  `((schema . ,test-schema)
    (payload . ((kind . test) (value . 42)))))

;; : (forall (a) (-> (-> String a) a))
(def (call-with-temporary-directory proc)
  (let (root (make-temporary-file-name "gerbil-parser-language-artifact"))
    (create-directory root)
    (try
     (proc root)
     (finally
      (when (file-exists? root)
        (delete-file-or-directory root #t))))))

;; : (-> Datum String)
(def (serialize value)
  (call-with-output-string (lambda (port) (write value port))))

;; : (-> String String String)
(def (write-serialized-sidecar root serialized)
  (let* ((digest (sha256-text serialized))
         (relative-path (compiled-language-artifact-relative-path digest))
         (path (path-expand relative-path root))
         (bytes (compress (string->utf8 serialized) compression: 9)))
    (create-directory* (path-directory path))
    (call-with-output-file
     path
     (lambda (port)
       (write-subu8vector bytes 0 (u8vector-length bytes) port)))
    relative-path))

;; : (-> String String Void)
(def (write-raw-file path content)
  (create-directory* (path-directory path))
  (call-with-output-file path (lambda (port) (write-string content port))))

(def language-artifact-tests
  (test-suite "compiled language artifact admission"
    (test-case "content identities use a portable physical filename"
      (check
       (sha256-identity-filename
        "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
       =>
       "sha256-0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
    (test-case "a content-addressed sidecar loads only from declared roots"
      (call-with-temporary-directory
       (lambda (root)
         (let* ((serialized (serialize test-value))
                (digest (sha256-text serialized))
                (relative-path (write-serialized-sidecar root serialized))
                (locator (list relative-path digest)))
           (check relative-path
                  => (string-append
                      "gerbil-parser/compiled-language-artifacts/sha256-"
                      (substring digest 7 71)
                      ".gir.z"))
           (check (load-compiled-language-artifact/roots
                   test-schema locator (list root))
                  => test-value)))))
    (test-case "an AOT image consumes the same compressed artifact without roots"
      (let* ((serialized (serialize test-value))
             (digest (sha256-text serialized))
             (locator
              (list (compiled-language-artifact-relative-path digest) digest))
             (encoded
              (base64-encode
               (compress (string->utf8 serialized) compression: 9))))
        (check (load-compiled-language-artifact/embedded
                test-schema locator encoded)
               => test-value)
        (check-exception
         (load-compiled-language-artifact/embedded
          test-schema locator "not-base64")
         true)))
    (test-case "locator identity prevents absolute and traversal reads"
      (call-with-temporary-directory
       (lambda (root)
         (let* ((serialized (serialize test-value))
                (digest (sha256-text serialized)))
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema (list "/tmp/not-an-artifact" digest) (list root))
            true)
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema (list "../not-an-artifact" digest) (list root))
            true)
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema
             (list "gerbil-parser/compiled-language-artifacts/not-a-digest.gir.z"
                   "not-a-digest")
             (list root))
            true)))))
    (test-case "missing and invalid zlib payloads fail closed"
      (call-with-temporary-directory
       (lambda (root)
         (let* ((serialized (serialize test-value))
                (digest (sha256-text serialized))
                (relative-path
                 (compiled-language-artifact-relative-path digest))
                (path (path-expand relative-path root))
                (locator (list relative-path digest)))
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema locator (list root))
            true)
           (write-raw-file path "not-zlib")
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema locator (list root))
            true)))))
    (test-case "digest, framing, and schema mismatches fail closed"
      (for-each
       (lambda (serialized expected-schema)
         (call-with-temporary-directory
          (lambda (root)
            (let* ((relative-path
                    (write-serialized-sidecar root serialized))
                   (locator (list relative-path (sha256-text serialized))))
              (check-exception
               (load-compiled-language-artifact/roots
                expected-schema locator (list root))
               true)))))
       (list (string-append (serialize test-value) " (trailing . datum)")
             (serialize '((schema . "different.v1") (payload . wrong))))
       (list test-schema test-schema))
      (call-with-temporary-directory
       (lambda (root)
         (let* ((serialized (serialize test-value))
                (wrong-digest (sha256-text "different bytes"))
                (relative-path
                 (compiled-language-artifact-relative-path wrong-digest))
                (path (path-expand relative-path root))
                (bytes (compress (string->utf8 serialized) compression: 9)))
           (create-directory* (path-directory path))
           (call-with-output-file
            path
            (lambda (port)
              (write-subu8vector bytes 0 (u8vector-length bytes) port)))
           (check-exception
            (load-compiled-language-artifact/roots
             test-schema (list relative-path wrong-digest) (list root))
            true)))))
    (test-case "materialization is deterministic and safe under competing writers"
      (call-with-temporary-directory
       (lambda (root)
         (let* ((serialized (serialize test-value))
                (digest (sha256-text serialized))
                (expected
                 (list (compiled-language-artifact-relative-path digest) digest))
                (barrier (make-barrier 12))
                (workers
                 (map (lambda (_)
                        (spawn
                         (lambda ()
                           (barrier-post! barrier)
                           (barrier-wait! barrier)
                           (materialize-compiled-language-artifact/output-dirs
                            test-value (list root)))))
                      (iota 12)))
                (results (map thread-join! workers)))
           (check results => (make-list 12 expected))
           (check (load-compiled-language-artifact/roots
                   test-schema expected (list root))
                  => test-value)))))
    (test-case "materialization refuses a corrupt immutable target"
      (call-with-temporary-directory
       (lambda (root)
         (let* ((locator
                 (materialize-compiled-language-artifact/output-dirs
                  test-value (list root)))
                (path (path-expand (car locator) root)))
           (write-raw-file path "corrupt")
           (check-exception
           (materialize-compiled-language-artifact/output-dirs
             test-value (list root))
            true)))))
    (test-case "parser generation is reused across fresh expansion calls"
      (call-with-temporary-directory
       (lambda (root)
         (let ((compile-count 0)
               (grammar
                '((schema . "gerbil-parser.grammar-ir.v1")
                  (grammar . cache-test)))
               (parser-ir
                '((schema . "gerbil-parser.parser-ir.v1")
                  (grammar . cache-test)))
               (materialized-parser-ir
                '((schema . "gerbil-parser.parser-ir.v1")
                  (materialization . aot-expansion)
                  (grammar . cache-test))))
           (def (compile)
             (set! compile-count (+ compile-count 1))
             parser-ir)
           (let-values (((first first-locator first-status)
                         (compile-language-parser-artifact/output-dirs
                          grammar compile (list root))))
             (let-values (((second second-locator second-status)
                           (compile-language-parser-artifact/output-dirs
                            grammar compile (list root))))
               (check first => materialized-parser-ir)
               (check second => materialized-parser-ir)
               (check first-locator => second-locator)
               (check first-status => 'miss)
               (check second-status => 'hit)
               (check compile-count => 1)))))))
    (test-case "complete declarations skip bound and parser regeneration"
      (call-with-temporary-directory
       (lambda (root)
         (let ((bound-count 0)
               (parser-count 0)
               (grammar
                '((schema . "gerbil-parser.grammar-ir.v1")
                  (grammar . declaration-cache-test))))
           (def (compile-bound)
             (set! bound-count (+ bound-count 1))
             '((schema . "gerbil-parser.bound-grammar-ir.v1")
               (grammar . declaration-cache-test)))
           (def (compile-parser)
             (set! parser-count (+ parser-count 1))
             '((schema . "gerbil-parser.parser-ir.v1")
               (grammar . declaration-cache-test)))
           (let-values (((first-grammar first-bound first-parser first-status)
                         (compile-language-declaration-artifacts/output-dirs
                          '(source-map-v1) grammar compile-bound compile-parser
                          (list root))))
             (let-values (((second-grammar second-bound second-parser second-status)
                           (compile-language-declaration-artifacts/output-dirs
                            '(source-map-v1) grammar compile-bound compile-parser
                            (list root))))
               (check first-grammar => second-grammar)
               (check first-bound => second-bound)
               (check first-parser => second-parser)
               (check first-status => 'miss)
               (check second-status => 'hit)
               (check bound-count => 1)
               (check parser-count => 1)
               (let-values (((_grammar _bound _parser changed-status)
                             (compile-language-declaration-artifacts/output-dirs
                              '(source-map-v2) grammar
                              compile-bound compile-parser (list root))))
                 (check changed-status => 'miss)
                 (check bound-count => 2)
                 ;; Parser IR is keyed only by canonical Grammar IR, so source
                 ;; ownership rebinding does not repeat LR generation.
                 (check parser-count => 1))))))))))

(export language-artifact-tests)
