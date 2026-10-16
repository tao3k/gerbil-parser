;;; Exact language ABI fixture parity is an ordinary Scheme semantic test.
(import :std/test :std/io/tempfile :std/misc/ports
        :std/encoding/json :std/encoding/hex
        (only-in ./generate-language-abi-alignment write-language-abi-alignment!))
(export language-abi-alignment-test)
(def (text-fixture-bytes)
  (call-with-output-u8vector
    (lambda (port)
      (for-each
        (lambda (row)
          (for-each
            (lambda (bytes)
              (let (length (u8vector-length bytes))
                (for-each (lambda (shift)
                            (write-u8 (bitwise-and 255 (arithmetic-shift length (- shift))) port))
                          '(0 8 16 24)))
              (write-u8vector bytes port))
            (append (map (lambda (key) (string->utf8 (hash-ref row key)))
                         '("name" "descriptor" "source"))
                    (list (hex-decode (hash-ref row "payload_hex")))))
          (write-u8 (if (hash-ref row "accepted") 1 0) port))
        (parameterize ((current-json-read-options
                         (make-JSONReadOptions object-as-hash: #t)))
          (call-with-input-file "rust/gerbil-parser-artifact/tests/fixtures/languages.json"
            read-json))))))
(def language-abi-alignment-test
  (test-suite "all-language ABI byte alignment"
    (test-case "accepted and rejected language products match the Rust fixture exactly"
      (let (directory (make-temporary-file-name "parser-language-alignment-"))
        (create-directory directory)
        (try
          (let (product (path-expand "language-artifacts.bin" directory))
            (write-language-abi-alignment! product)
            (check (equal? (call-with-input-file product read-all-as-u8vector)
                           (text-fixture-bytes)) => #t))
          (finally (delete-file-or-directory directory #t)))))))
