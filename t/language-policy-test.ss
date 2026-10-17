(import :gerbil-parser/language-test-support)
;;; Descriptor-bound policy parity across loader, contextual C ABI and AOT admission.
(import (only-in :gerbil-parser/t/abi-datum-support abi-datum-read)
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
                 bind-language-abi abi-descriptor-payload
                 abi-parse-binary-payload abi-parse-binary-payload/bytes)
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

(deflanguage-parser-tests language-policy-test "descriptor-bound language policy"
  (engine)
    (scenario "admission rejects malformed policy configuration"
      (each (row) ((list (list #f "test.v1" 17 void)
             (list records-language-grammar "" 17 void)
             (list records-language-grammar "test.v1" 0 void)
             (list records-language-grammar "test.v1" 1.5 void)
             (list records-language-grammar "test.v1" 17 #f)))
         (raises (apply language-grammar-with-parser-policy row) true)))
    (property "policy is retained by observability configuration"
      (bindings)
      (equal (language-grammar-parser-policy
              (language-grammar-with-observability descriptor #f))
              (language-grammar-parser-policy descriptor)))
    (property "public loader and descriptor reject the same lossless artifact"
      (bindings (source "α=1\r\n") (a (parse-records-policy source)) (b ((.ref policy-loader '.parse) source)) (c (parse-language-source descriptor source)))
      (equal a  b)
      (equal b  c)
      (equal (parse-artifact-success? a)  #f)
      (equal (parse-artifact-valid? a)  #t)
      (equal (parse-artifact-roundtrip a)  source)
      (equal (.ref policy-loader 'capabilities)  '(grammar-ir parser-ir scheme scheme-policy)))
    (scenario "budget is scoped and checker runs once only after recognition succeeds"
      (parameters ((current-lr-branch-budget 123))
        (scope ((before calls))
          (act (parse-records-policy "a=1\n"))
          (equal calls  (+ before 1))
          (equal seen-budget  17)
          (equal (current-lr-branch-budget)  123)
          (act (parse-records-policy "!"))
          (equal calls  (+ before 1)))
        (raises
         (call-with-language-parser-policy descriptor "" (lambda () (error "recognition failed"))) true)
        (equal (current-lr-branch-budget)  123)))
    (scenario "validator exceptions restore the caller budget"
      (scope ((throwing (language-grammar-with-parser-policy
                      records-language-grammar "records.throw.v1" 17
                      (lambda (_) (error "validator failed")))))
        (parameters ((current-lr-branch-budget 123))
          (raises (parse-language-source throwing "a=1\n") true)
          (equal (current-lr-branch-budget)  123))))
    (property "validator acceptance preserves the canonical artifact"
      (bindings (source "β=1\n") (accepting (language-grammar-with-parser-policy
                         records-language-grammar "records.accept.v1" 17 (lambda (_) #f))))
      (equal (parse-language-source accepting source)
                (parse-language-source records-language-grammar source)))
    (scenario "invalid diagnostics escape rather than publish invalid artifacts"
      (scope ((invalid (language-grammar-with-parser-policy
                     records-language-grammar "records.invalid.v1" 17 (lambda (_) 'invalid))))
        (raises (parse-language-source invalid "a=1\n") true)))
    (scenario "contextual native bytes cannot bypass the policy checker"
      (each (product) ((list #f records-contextual-product))
         (sequential ((language (bind-language-abi descriptor product))
                (source "α=1\n") (before calls)
                (payload (abi-parse-binary-payload/bytes language (string->utf8 source))))
           (equal calls  (+ before 1))
           (equal (u8vector-u32-ref payload 8 little)  1)
           (equal payload  (abi-parse-binary-payload language source))
           (sequential ((metadata (abi-datum-read (abi-descriptor-payload language)
                                         ))
                  (policy (hash-get metadata "parserPolicy")))
             (equal (hash-get policy "identity")  "records.test-policy.v1")
             (equal (hash-get policy "branchBudget")  17)
             (equal (hash-get policy "portable")  #f)))))
    (scenario "descriptor source admission retains portable recognition bytes"
      (raises
       (language-rust-runtime-module-source descriptor)
       (lambda (e)
         (equal? (error-message e)
                 "language parser policy is unsupported by standalone Rust AOT")))
      (equal (language-rust-runtime-module-source records-language-grammar)
              (rust-runtime-module-source
                 (language-grammar-language records-language-grammar)
                 (language-grammar-version records-language-grammar)
                 (language-grammar-contract records-language-grammar)
                 (parser-machine-grammar-digest (language-grammar-machine records-language-grammar))
                 (language-grammar-ir records-language-grammar))))
    (scenario "contextual Rust rejects policy before admission and output publication"
      (scope ((path (path-expand "contextual-language-policy-rejected.rs" (getenv "TMPDIR" "/tmp"))))
        (equal (file-exists? path)  #f)
        (wind
          (before (act (void)))
          (body (each (product) ((list records-contextual-product #f))
              (raises
               (generate-contextual-language-rust-runtime-module path descriptor product)
               (lambda (e)
                 (equal? (error-message e)
                         "language parser policy is unsupported by standalone Rust AOT")))
              (equal (file-exists? path)  #f)))
          (after (when (file-exists? path) (act (delete-file path)))))))
    (scenario "policy rejection preserves an existing downstream product"
      (scope ((path (path-expand "language-policy-existing.rs" (getenv "TMPDIR" "/tmp")))
            (sentinel "existing downstream product α"))
        (equal (file-exists? path)  #f)
        (wind (before (act (call-with-output-file path (lambda (port) (display sentinel port))))) (body
           (each (emit) ((list (lambda () (generate-language-rust-runtime-module path descriptor))
                  (lambda () (generate-contextual-language-rust-runtime-module path descriptor records-contextual-product))
                  (lambda () (generate-contextual-language-rust-runtime-module path descriptor #f))))
              (raises (emit)
                               (lambda (e)
                                 (equal? (error-message e)
                                         "language parser policy is unsupported by standalone Rust AOT")))
              (equal (call-with-input-file path read-line)  sentinel))) (after (act (delete-file path))))))
    (scenario "standalone Rust rejects an unlowered policy before writing output"
      (scope ((path (path-expand "language-policy-rejected.rs" (getenv "TMPDIR" "/tmp"))))
        (equal (file-exists? path)  #f)
        (raises (generate-language-rust-runtime-module path descriptor)
                         (lambda (e)
                           (equal? (error-message e)
                                   "language parser policy is unsupported by standalone Rust AOT")))
        (equal (file-exists? path)  #f)
        (equal (require-portable-language-policy! records-language-grammar)
                records-language-grammar))))
