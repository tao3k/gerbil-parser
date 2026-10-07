;;; Descriptor-bound policy parity across loader, contextual C ABI and AOT admission.
(import (only-in :gerbil-parser/t/native-datum-support native-datum-read)
        :std/test
        (only-in :std/vector/u8vector little u8vector-u32-ref)
        (only-in :clan/poo/object .ref)
        (only-in :gerbil-parser/language-support/development
                 deflanguage-development-loader parse-language-source call-with-language-parser-policy)
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-with-parser-policy language-grammar-parser-policy
                 language-grammar-with-observability require-portable-language-policy!
                 language-grammar-language language-grammar-version language-grammar-contract
                 language-grammar-machine language-grammar-ir)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-branch-budget)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip
                 parse-artifact-ref)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec
                 bind-native-language native-descriptor-payload
                 native-parse-binary-payload native-parse-binary-payload/bytes)
        (only-in :gerbil-parser/src/compiler/rust-runtime
                 generate-language-rust-runtime-module language-rust-runtime-module-source
                 rust-runtime-module-source)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/compiler/rust-scanner generate-contextual-language-rust-runtime-module)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records
                 records-language-grammar records-contextual-product))

(def calls 0)
(def seen-budget #f)
(def diagnostic
  '((schema . "gerbil-parser.diagnostic.v1") (code . "POLICY-TEST")
    (reasonKind . parse-rejected) (failureKind . policy-rejected)
    (message . "rejected by test language policy") (byteOffset . 0)))
(def descriptor
  (language-grammar-with-parser-policy
   records-language-grammar "records.test-policy.v1" 17
   (lambda (_cst)
     (set! calls (+ calls 1))
     (set! seen-budget (current-lr-branch-budget))
     diagnostic)))
(deflanguage-development-loader policy-loader (grammar descriptor) (parse parse-records-policy))

(def language-policy-test
  (test-suite "descriptor-bound language policy"
    (test-case "admission rejects malformed policy configuration"
      (for-each
       (lambda (row)
         (check-exception (apply language-grammar-with-parser-policy row) true))
       (list (list #f "test.v1" 17 void)
             (list records-language-grammar "" 17 void)
             (list records-language-grammar "test.v1" 0 void)
             (list records-language-grammar "test.v1" 1.5 void)
             (list records-language-grammar "test.v1" 17 #f))))
    (test-case "policy is retained by observability configuration"
      (check (language-grammar-parser-policy
              (language-grammar-with-observability descriptor #f))
             => (language-grammar-parser-policy descriptor)))
    (test-case "public loader and descriptor reject the same lossless artifact"
      (let* ((source "α=1\r\n")
             (a (parse-records-policy source))
             (b ((.ref policy-loader '.parse) source))
             (c (parse-language-source descriptor source)))
        (check a => b) (check b => c)
        (check (parse-artifact-success? a) => #f)
        (check (parse-artifact-valid? a) => #t)
        (check (parse-artifact-roundtrip a) => source)
        (check (.ref policy-loader 'capabilities) => '(grammar-ir parser-ir scheme scheme-policy))))
    (test-case "budget is scoped and checker runs once only after recognition succeeds"
      (parameterize ((current-lr-branch-budget 123))
        (let (before calls)
          (parse-records-policy "a=1\n")
          (check calls => (+ before 1))
          (check seen-budget => 17)
          (check (current-lr-branch-budget) => 123)
          (parse-records-policy "!")
          (check calls => (+ before 1)))
        (check-exception
         (call-with-language-parser-policy descriptor "" (lambda () (error "recognition failed"))) true)
        (check (current-lr-branch-budget) => 123)))
    (test-case "validator exceptions restore the caller budget"
      (let (throwing (language-grammar-with-parser-policy
                      records-language-grammar "records.throw.v1" 17
                      (lambda (_) (error "validator failed"))))
        (parameterize ((current-lr-branch-budget 123))
          (check-exception (parse-language-source throwing "a=1\n") true)
          (check (current-lr-branch-budget) => 123))))
    (test-case "validator acceptance preserves the canonical artifact"
      (let* ((source "β=1\n")
             (accepting (language-grammar-with-parser-policy
                         records-language-grammar "records.accept.v1" 17 (lambda (_) #f))))
        (check (parse-language-source accepting source)
               => (parse-language-source records-language-grammar source))))
    (test-case "invalid diagnostics escape rather than publish invalid artifacts"
      (let (invalid (language-grammar-with-parser-policy
                     records-language-grammar "records.invalid.v1" 17 (lambda (_) 'invalid)))
        (check-exception (parse-language-source invalid "a=1\n") true)))
    (test-case "contextual native bytes cannot bypass the policy checker"
      (for-each
       (lambda (product)
         (let* ((language (bind-native-language descriptor product))
                (source "α=1\n") (before calls)
                (payload (native-parse-binary-payload/bytes language (string->utf8 source))))
           (check calls => (+ before 1))
           (check (u8vector-u32-ref payload 8 little) => 1)
           (check payload => (native-parse-binary-payload language source))
           (let* ((metadata (native-datum-read (native-descriptor-payload language)
                                         ))
                  (policy (hash-get metadata "parserPolicy")))
             (check (hash-get policy "identity") => "records.test-policy.v1")
             (check (hash-get policy "branchBudget") => 17)
             (check (hash-get policy "portable") => #f))))
       (list #f records-contextual-product)))
    (test-case "descriptor source admission retains portable recognition bytes"
      (check-exception
       (language-rust-runtime-module-source descriptor)
       (lambda (e)
         (equal? (error-message e)
                 "language parser policy is unsupported by standalone Rust AOT")))
      (check (language-rust-runtime-module-source records-language-grammar)
             => (rust-runtime-module-source
                 (language-grammar-language records-language-grammar)
                 (language-grammar-version records-language-grammar)
                 (language-grammar-contract records-language-grammar)
                 (parser-machine-grammar-digest (language-grammar-machine records-language-grammar))
                 (language-grammar-ir records-language-grammar))))
    (test-case "contextual Rust rejects policy before admission and output publication"
      (let (path (path-expand "contextual-language-policy-rejected.rs" (getenv "TMPDIR" "/tmp")))
        (check (file-exists? path) => #f)
        (dynamic-wind
         void
         (lambda ()
           (for-each
            (lambda (product)
              (check-exception
               (generate-contextual-language-rust-runtime-module path descriptor product)
               (lambda (e)
                 (equal? (error-message e)
                         "language parser policy is unsupported by standalone Rust AOT")))
              (check (file-exists? path) => #f))
            (list records-contextual-product #f)))
         (lambda () (when (file-exists? path) (delete-file path))))))
    (test-case "policy rejection preserves an existing downstream product"
      (let ((path (path-expand "language-policy-existing.rs" (getenv "TMPDIR" "/tmp")))
            (sentinel "existing downstream product α"))
        (check (file-exists? path) => #f)
        (dynamic-wind
         (lambda () (call-with-output-file path (lambda (port) (display sentinel port))))
         (lambda ()
           (for-each
            (lambda (emit)
              (check-exception (emit)
                               (lambda (e)
                                 (equal? (error-message e)
                                         "language parser policy is unsupported by standalone Rust AOT")))
              (check (call-with-input-file path read-line) => sentinel))
            (list (lambda () (generate-language-rust-runtime-module path descriptor))
                  (lambda () (generate-contextual-language-rust-runtime-module path descriptor records-contextual-product))
                  (lambda () (generate-contextual-language-rust-runtime-module path descriptor #f)))))
         (lambda () (delete-file path)))))
    (test-case "standalone Rust rejects an unlowered policy before writing output"
      (let (path (path-expand "language-policy-rejected.rs" (getenv "TMPDIR" "/tmp")))
        (check (file-exists? path) => #f)
        (check-exception (generate-language-rust-runtime-module path descriptor)
                         (lambda (e)
                           (equal? (error-message e)
                                   "language parser policy is unsupported by standalone Rust AOT")))
        (check (file-exists? path) => #f)
        (check (require-portable-language-policy! records-language-grammar)
               => records-language-grammar)))))
(export language-policy-test)
