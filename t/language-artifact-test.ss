#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Independent positive, negative, and concurrent admission for AOT IR files.

(import (only-in :gerbil-parser/src/schema compiler-generator-metadata)
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/sync/barrier
                 barrier-post! barrier-wait! make-barrier)
        (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/src/runtime/embedded-image
                 pack-language-artifact-image unpack-language-artifact-image)
        (only-in :std/encoding/zlib compress)
        (only-in :std/misc/ports read-all-as-u8vector)
        (only-in :gerbil-parser/src/compiler/language-artifact
                 compile-language-declaration-artifacts/output-dirs
                 make-admitted-language-declaration compile-admitted-language-declaration
                 compile-language-parser-artifact/output-dirs
                 materialize-compiled-language-artifact/output-dirs)
        (only-in :gerbil-parser/src/compiler/parser-ir compile-parser)
        (only-in :gerbil-parser/src/compiler/lr current-grammar-source-map)
        (only-in :gerbil-parser/src/runtime/identity sha256-bytes sha256-text)
        (only-in :gerbil-parser/src/runtime/language-artifact
                 compiled-language-artifact-relative-path
                 load-compiled-language-artifact/embedded
                 load-compiled-language-artifact/roots
                 sha256-identity-filename))


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

(deflanguage-parser-tests language-artifact-tests "compiled language artifact admission"
  (engine)
    (scenario "Unicode publication preserves text identity and compressed bytes across both loaders"
      (using (call-with-temporary-directory) (root)
         (sequential ((value `((schema . ,test-schema)
                        (payload . (中文 λ "line\r\nvalue" "𝄞"))))
                (expected-bytes (string->utf8 (serialize value)))
                (expected-digest (sha256-text (serialize value)))
                (expected-compressed (compress expected-bytes compression: 9))
                (locator (materialize-compiled-language-artifact/output-dirs value (list root)))
                (path (path-expand (car locator) root)))
           (equal locator  (list (compiled-language-artifact-relative-path expected-digest)
                                  expected-digest))
           (equal (call-with-input-file path read-all-as-u8vector)  expected-compressed)
           (equal (load-compiled-language-artifact/roots test-schema locator (list root))  value)
           (equal (load-compiled-language-artifact/embedded
                   test-schema locator (pack-language-artifact-image expected-compressed))  value)
           (equal (materialize-compiled-language-artifact/output-dirs value (list root))  locator)
           (equal (call-with-input-file path read-all-as-u8vector)  expected-compressed))))
    (scenario "authenticated artifacts reject BOM framing and malformed UTF-8 in trailing comments"
      (scope ((datum (string->utf8 (serialize test-value))))
        (each (bytes) ((list (u8vector-append #u8(239 187 191) datum)
               (u8vector-append datum (string->utf8 " ;") #u8(255))
               (u8vector-append datum (string->utf8 " ;") #u8(192 128))
               (u8vector-append datum (string->utf8 " ;") #u8(226 130))))
           (sequential ((digest (sha256-bytes bytes))
                  (locator (list (compiled-language-artifact-relative-path digest) digest))
                  (image (pack-language-artifact-image (compress bytes compression: 9))))
             (raises
              (load-compiled-language-artifact/embedded test-schema locator image) exception?)))))
    (scenario "declaration compiler passes selected origins into LR rejection and restores context"
      (using (call-with-temporary-directory) (root)
         (sequential ((grammar '((schema . "gerbil-parser.grammar-ir.v1")
                           (grammar . nullable-origin)
                           (syntax-kinds (Start node ())) (terminals) (lexical-rules)
                           (rules (start (alias Start (repeat (reference item)))) (item (empty)))
                           (extras) (parser-entrypoints (start Start))
                           (flow (source lexical) (lexical cst))))
                (source-map '((rule (start (componentOwner . selected-base))
                                   (item (componentOwner . selected-override)))))
                (admitted (make-admitted-language-declaration grammar 'test-origin '() source-map))
                (condition
                 (with-catch (lambda (condition) condition)
                   (lambda ()
                     (compile-admitted-language-declaration
                      admitted compile-parser
                      (lambda (_grammar _origin _lineage _sources)
                        '((schema . "gerbil-parser.bound-grammar-ir.v1")))
                      output-dirs: (list root)) #f))))
           (equal (error-message condition)  "resolved repetition operand accepts empty input")
           (sequential ((details (list-ref (error-irritants condition) 3))
                  (references (cdr (assq 'nullableReferencePath details))))
             (equal (cdr (assq 'componentOwner (cdr (assq 'source (car references)))))
                     'selected-override))
           (equal (current-grammar-source-map)  '()))))
    (scenario "complete cached declarations cannot bypass resolved repetition admission"
      (using (call-with-temporary-directory) (root)
         (sequential ((grammar '((schema . "gerbil-parser.grammar-ir.v1") (grammar . stale-admission)
                           (rules (start (repeat (reference item))) (item (empty)))))
                (source-map '((rule (item (componentOwner . cached-override)))))
                (origin 'test-origin) (lineage '())
                (admitted (make-admitted-language-declaration grammar origin lineage source-map)))
           ;; Seed a complete old-generation receipt with its unchanged nullable facts.
           (act (compile-language-declaration-artifacts/output-dirs
            (list origin lineage source-map) grammar
            (lambda () '((schema . "gerbil-parser.bound-grammar-ir.v1")))
            (lambda () '((schema . "gerbil-parser.parser-ir.v1")
                          (lr-spec (schema . "gerbil-parser.lr-spec.v1") (nullable item start))))
            (list root)))
           (scope ((calls 0)
                 (condition #f))
             (act (set! condition
               (with-catch (lambda (condition) condition)
                 (lambda ()
                   (compile-admitted-language-declaration admitted
                    (lambda (_) (set! calls (+ calls 1)) (error "unexpected generation"))
                    (lambda (_grammar _origin _lineage _sources)
                      (set! calls (+ calls 1)) (error "unexpected binding"))
                    output-dirs: (list root)) #f))))
             (equal calls  0)
             (equal (error-message condition)  "resolved repetition operand accepts empty input")
             (equal (current-grammar-source-map)  '())))))
    (property "content identities use a portable physical filename"
      (bindings)
      (equal
       (sha256-identity-filename
        "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

       "sha256-0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
    (scenario "a content-addressed sidecar loads only from declared roots"
      (using (call-with-temporary-directory) (root)
         (sequential ((serialized (serialize test-value))
                (digest (sha256-text serialized))
                (relative-path (write-serialized-sidecar root serialized))
                (locator (list relative-path digest)))
           (equal relative-path
                   (string-append
                      "gerbil-parser/compiled-language-artifacts/sha256-"
                      (substring digest 7 71)
                      ".gir.z"))
           (equal (load-compiled-language-artifact/roots
                   test-schema locator (list root))
                   test-value))))
    (scenario "native image framing preserves every tail length and unsigned word"
      (each (size) ('(0 1 2 3 4 5 6 7 8 9 15 16 17 255 256 257
         4095 4096 4097 8191 8192 8193 65535 65536 65537))
         (scope ((bytes (make-u8vector size)))
           (iterate loop ((index 0))
             (when (< index size)
               (act (u8vector-set! bytes index (modulo index 256)))
               (continue loop (fx+ index 1))))
           (equal (unpack-language-artifact-image (pack-language-artifact-image bytes))
                   bytes)))
      (equal (pack-language-artifact-image #u8(1 2 3 4 5 6 7 8))
              '#u64(8 #x0102030405060708))
      (equal (unpack-language-artifact-image '#u64(8 #xffffffffffffffff))
              #u8(255 255 255 255 255 255 255 255))
      (each (invalid) ((list "encoded" #u8(1) '#u64() '#u64(1) '#u64(0 0)
             '#u64(#xffffffffffffffff) '#u64(1 #x0100000000000001)))
         (raises (unpack-language-artifact-image invalid) true)))
    (scenario "native producer preserves high-bit full words and partial tails"
      (equal (pack-language-artifact-image #u8(255))  '#u64(1 #xff00000000000000))
      (equal (pack-language-artifact-image #u8(255 254 253 252 251 250 249))
              '#u64(7 #xfffefdfcfbfaf900))
      (equal (pack-language-artifact-image #u8(255 255 255 255 255 255 255 255 128))
              '#u64(9 #xffffffffffffffff #x8000000000000000))
      (sequential ((bytes (make-u8vector 17 255))
             (image (pack-language-artifact-image bytes)))
        (act (u8vector-set! bytes 0 0))
        (equal (unpack-language-artifact-image image)  (make-u8vector 17 255))))
    (scenario "an AOT image consumes the same compressed artifact without roots"
      (sequential ((serialized (serialize test-value))
             (digest (sha256-text serialized))
             (locator
              (list (compiled-language-artifact-relative-path digest) digest))
             (encoded
              (pack-language-artifact-image
               (compress (string->utf8 serialized) compression: 9))))
        (equal (load-compiled-language-artifact/embedded
                test-schema locator encoded)
                test-value)
        (raises
         (load-compiled-language-artifact/embedded
          test-schema locator "not-a-native-image")
         true)))
    (scenario "byte hashing preserves Unicode artifact identity in sidecars and AOT images"
      (sequential ((value `((schema . ,test-schema) (payload . "λ中😀é\x0;")))
             (serialized (serialize value)) (bytes (string->utf8 serialized))
             (digest (sha256-text serialized))
             (locator (list (compiled-language-artifact-relative-path digest) digest)))
        (equal (sha256-bytes bytes)  digest)
        (equal (load-compiled-language-artifact/embedded
                test-schema locator (pack-language-artifact-image (compress bytes compression: 9)))  value)
        (using (call-with-temporary-directory) (root)
           (act (write-serialized-sidecar root serialized))
           (equal (load-compiled-language-artifact/roots test-schema locator (list root))  value))))
    (scenario "a matching raw digest cannot admit malformed UTF-8 inside an artifact string"
      (each (invalid) ('(#u8(255) #u8(192 175) #u8(237 160 128) #u8(244 144 128 128) #u8(226 130)))
         (sequential ((prefix (string->utf8
                         (string-append "((schema . " (object->string test-schema) ") (payload . \"")))
                (bytes (u8vector-append prefix invalid #u8(34 41 41)))
                (digest (sha256-bytes bytes))
                (locator (list (compiled-language-artifact-relative-path digest) digest)))
           (raises
            (load-compiled-language-artifact/embedded
             test-schema locator (pack-language-artifact-image (compress bytes compression: 9))) true))))
    (scenario "locator identity prevents absolute and traversal reads"
      (using (call-with-temporary-directory) (root)
         (sequential ((serialized (serialize test-value))
                (digest (sha256-text serialized)))
           (raises
            (load-compiled-language-artifact/roots
             test-schema (list "/tmp/not-an-artifact" digest) (list root))
            true)
           (raises
            (load-compiled-language-artifact/roots
             test-schema (list "../not-an-artifact" digest) (list root))
            true)
           (raises
            (load-compiled-language-artifact/roots
             test-schema
             (list "gerbil-parser/compiled-language-artifacts/not-a-digest.gir.z"
                   "not-a-digest")
             (list root))
            true))))
    (scenario "missing and invalid zlib payloads fail closed"
      (using (call-with-temporary-directory) (root)
         (sequential ((serialized (serialize test-value))
                (digest (sha256-text serialized))
                (relative-path
                 (compiled-language-artifact-relative-path digest))
                (path (path-expand relative-path root))
                (locator (list relative-path digest)))
           (raises
            (load-compiled-language-artifact/roots
             test-schema locator (list root))
            true)
           (act (write-raw-file path "not-zlib"))
           (raises
            (load-compiled-language-artifact/roots
             test-schema locator (list root))
            true))))
    (scenario "digest, framing, and schema mismatches fail closed"
      (each (serialized expected-schema) ((list (string-append (serialize test-value) " (trailing . datum)")
             (serialize '((schema . "different.v1") (payload . wrong)))) (list test-schema test-schema))
         (using (call-with-temporary-directory) (root)
            (sequential ((relative-path
                    (write-serialized-sidecar root serialized))
                   (locator (list relative-path (sha256-text serialized))))
              (raises
               (load-compiled-language-artifact/roots
                expected-schema locator (list root))
               true))))
      (using (call-with-temporary-directory) (root)
         (sequential ((serialized (serialize test-value))
                (wrong-digest (sha256-text "different bytes"))
                (relative-path
                 (compiled-language-artifact-relative-path wrong-digest))
                (path (path-expand relative-path root))
                (bytes (compress (string->utf8 serialized) compression: 9)))
           (act (create-directory* (path-directory path)))
           (act (call-with-output-file
            path
            (lambda (port)
              (write-subu8vector bytes 0 (u8vector-length bytes) port))))
           (raises
            (load-compiled-language-artifact/roots
             test-schema (list relative-path wrong-digest) (list root))
            true))))
    (scenario "materialization is deterministic and safe under competing writers"
      (using (call-with-temporary-directory) (root)
         (sequential ((serialized (serialize test-value))
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
           (equal results  (make-list 12 expected))
           (equal (load-compiled-language-artifact/roots
                   test-schema expected (list root))
                   test-value))))
    (scenario "materialization refuses a corrupt immutable target"
      (using (call-with-temporary-directory) (root)
         (sequential ((locator
                 (materialize-compiled-language-artifact/output-dirs
                  test-value (list root)))
                (path (path-expand (car locator) root)))
           (act (write-raw-file path "corrupt"))
           (raises
           (materialize-compiled-language-artifact/output-dirs
             test-value (list root))
            true))))
    (scenario "generator identity is stable and revision metadata is admitted on reuse"
      (scope ((parser (compiler-generator-metadata 'parser))
            (declaration (compiler-generator-metadata 'declaration)))
        (equal (cdr (assq 'name parser))  'gerbil-parser/lalr-generator)
        (equal (cdr (assq 'name declaration))  'gerbil-parser/language-declaration-generator)
        (equal (cdr parser)  (cdr declaration))
        (equal (cdr (assq 'repetition-owner parser))  'exclusive)
        (equal (cdr (assq 'partition-conflict-origin parser))  'surviving-member)
        (raises (compiler-generator-metadata 'unknown) exception?))
      (using (call-with-temporary-directory) (root)
         (scope ((grammar '((schema . "gerbil-parser.grammar-ir.v1") (grammar . metadata-control)))
               (calls 0))
(locals ((compile (lambda ()
             (set! calls (+ calls 1))
             '((schema . "gerbil-parser.parser-ir.v1") (grammar . metadata-control)))))

           (scope-values (((_value _locator status)
                         (compile-language-parser-artifact/output-dirs grammar compile (list root))))
             (equal status  'miss))
           (sequential ((directory (path-expand "gerbil-parser/compiled-language-parser-cache" root))
                  (path (path-expand (car (directory-files directory)) directory))
                  (receipt (call-with-input-file path read)))
             (equal (cdr (assq 'generator receipt))  (compiler-generator-metadata 'parser))
             (equal (assq 'generatorContract receipt)  #f)
             (act (write-raw-file path
               (serialize (map (lambda (row)
                                 (if (eq? (car row) 'generator)
                                   (cons 'generator
                                     (map (lambda (feature)
                                            (if (eq? (car feature) 'repetition-owner)
                                              '(repetition-owner . shared) feature)) (cdr row))) row)) receipt))))
             (raises
               (compile-language-parser-artifact/output-dirs grammar compile (list root)) exception?)
             (equal calls  1)
             ;; A pre-repair packing revision cannot qualify a warm artifact.
             (act (write-raw-file path
               (serialize (map (lambda (row)
                 (if (eq? (car row) 'generator)
                   (cons 'generator (filter (lambda (feature)
                     (not (eq? (car feature) 'partition-conflict-origin))) (cdr row))) row)) receipt))))
             (raises
               (compile-language-parser-artifact/output-dirs grammar compile (list root)) exception?)
             (equal calls  1)
             (act (write-raw-file path (serialize receipt)))
             (scope-values (((_parser _path reuse)
                            (compile-language-parser-artifact/output-dirs grammar compile (list root))))
               (equal reuse  'hit))
             (equal calls  1))))))
    (scenario "pre-isolation generator receipts cannot bypass the repaired lowering"
      (using (call-with-temporary-directory) (root)
         (sequential ((grammar '((schema . "gerbil-parser.grammar-ir.v1") (grammar . old-generator)))
                (identity '(source-map-v1))
                (parser-count 0) (bound-count 0))
(locals ((seed (lambda ( directory contract inputs fields)
             (let* ((key (sha256-text (call-with-output-string
                                       (lambda (port) (write (cons contract inputs) port)))))
                    (path (path-expand (string-append "gerbil-parser/" directory "/"
                                           (sha256-identity-filename key) ".scm") root)))
               (create-directory* (path-directory path))
               (call-with-output-file path
                 (lambda (port)
                   (write (append `((schema . ,(if (equal? directory "compiled-language-parser-cache")
                                                  "gerbil-parser.parser-artifact-cache.v1"
                                                  "gerbil-parser.language-declaration-cache.v1"))
                                    (key . ,key) (generatorContract . ,contract)) fields) port)))
               path)))
(parser (lambda ()
             (set! parser-count (+ parser-count 1))
             '((schema . "gerbil-parser.parser-ir.v1") (grammar . new-generator))))
(bound (lambda ()
             (set! bound-count (+ bound-count 1))
             '((schema . "gerbil-parser.bound-grammar-ir.v1") (grammar . new-generator)))))


           (scope ((old-parser (seed "compiled-language-parser-cache" "gerbil-parser.lalr1-generator.v3"
                                  (list grammar) '((artifact . stale-parser))))
                 (old-declaration (seed "compiled-language-declaration-cache"
                                       "gerbil-parser.language-declaration-generator.v3"
                                       (list identity grammar)
                                       '((grammar . stale-grammar) (bound . stale-bound) (parser . stale-parser)))))
             (scope-values (((value _locator status)
                           (compile-language-parser-artifact/output-dirs grammar parser (list root))))
               (equal status  'miss)
               (equal (cdr (assq 'grammar value))  'new-generator))
             (scope-values (((_grammar _bound _parser status)
                           (compile-language-declaration-artifacts/output-dirs identity grammar bound parser (list root))))
               (equal status  'miss))
             (equal parser-count  1)
             (equal bound-count  1)
             (equal (file-exists? old-parser)  #t)
             (equal (file-exists? old-declaration)  #t))))))
    (scenario "parser generation is reused across fresh expansion calls"
      (using (call-with-temporary-directory) (root)
         (scope ((compile-count 0)
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
(locals ((compile (lambda ()
             (set! compile-count (+ compile-count 1))
             parser-ir)))

           (scope-values (((first first-locator first-status)
                         (compile-language-parser-artifact/output-dirs
                          grammar compile (list root))))
             (scope-values (((second second-locator second-status)
                           (compile-language-parser-artifact/output-dirs
                            grammar compile (list root))))
               (equal first  materialized-parser-ir)
               (equal second  materialized-parser-ir)
               (equal first-locator  second-locator)
               (equal first-status  'miss)
               (equal second-status  'hit)
               (equal compile-count  1)))))))
    (scenario "complete declarations skip bound and parser regeneration"
      (using (call-with-temporary-directory) (root)
         (scope ((bound-count 0)
               (parser-count 0)
               (grammar
                '((schema . "gerbil-parser.grammar-ir.v1")
                  (grammar . declaration-cache-test))))
(locals ((compile-bound (lambda ()
             (set! bound-count (+ bound-count 1))
             '((schema . "gerbil-parser.bound-grammar-ir.v1")
               (grammar . declaration-cache-test))))
(compile-parser (lambda ()
             (set! parser-count (+ parser-count 1))
             '((schema . "gerbil-parser.parser-ir.v1")
               (grammar . declaration-cache-test)))))


           (scope-values (((first-grammar first-bound first-parser first-status)
                         (compile-language-declaration-artifacts/output-dirs
                          '(source-map-v1) grammar compile-bound compile-parser
                          (list root))))
             (scope-values (((second-grammar second-bound second-parser second-status)
                           (compile-language-declaration-artifacts/output-dirs
                            '(source-map-v1) grammar compile-bound compile-parser
                            (list root))))
               (equal first-grammar  second-grammar)
               (equal first-bound  second-bound)
               (equal first-parser  second-parser)
               (equal first-status  'miss)
               (equal second-status  'hit)
               (equal bound-count  1)
               (equal parser-count  1)
               (scope-values (((_grammar _bound _parser changed-status)
                             (compile-language-declaration-artifacts/output-dirs
                              '(source-map-v2) grammar
                              compile-bound compile-parser (list root))))
                 (equal changed-status  'miss)
                 (equal bound-count  2)
                 ;; Parser IR is keyed only by canonical Grammar IR, so source
                 ;; ownership rebinding does not repeat LR generation.
                 (equal parser-count  1)))))))))


;; gxtest discovers only exported names ending in -test.
(def language-artifact-test language-artifact-tests)
(export language-artifact-test)
