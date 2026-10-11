;;; Exact language ABI fixture parity is an ordinary Scheme semantic test.
(import :std/test :std/io/tempfile :std/misc/ports
        (only-in ./generate-language-abi-alignment write-language-abi-alignment!))
(export language-abi-alignment-test)
(def language-abi-alignment-test
  (test-suite "all-language ABI byte alignment"
    (test-case "accepted and rejected language products match the Rust fixture exactly"
      (let (directory (make-temporary-file-name "parser-language-alignment-"))
        (create-directory directory)
        (try
          (let (product (path-expand "language-artifacts.bin" directory))
            (write-language-abi-alignment! product)
            (check (equal? (call-with-input-file product read-all-as-u8vector)
                           (call-with-input-file "rust/gerbil-parser-artifact/tests/fixtures/languages.bin"
                             read-all-as-u8vector)) => #t))
          (finally (delete-file-or-directory directory #t)))))))
