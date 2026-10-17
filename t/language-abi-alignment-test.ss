(import :gerbil-parser/language-test-support)
;;; Exact language ABI fixture parity is an ordinary Scheme semantic test.
(import :std/test :std/io/tempfile :std/misc/ports
        :std/encoding/json :std/encoding/hex
        (only-in ./generate-language-abi-alignment write-language-abi-alignment! report-language-abi-mismatch!))

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
(deflanguage-parser-tests language-abi-alignment-test "all-language ABI byte alignment"
  (engine)
    (scenario "accepted and rejected language products match the Rust fixture exactly"
      (scope ((directory (make-temporary-file-name "parser-language-alignment-")))
        (act (create-directory directory))
        (protect (body
          (scope ((product (path-expand "language-artifacts.bin" directory)))
            (act (write-language-abi-alignment! product))
            (sequential ((actual (call-with-input-file product read-all-as-u8vector))
                   (expected (text-fixture-bytes)))
              (unless (equal? actual expected)
                (act (report-language-abi-mismatch! actual expected)))
              (equal (equal? actual expected)  #t)))
          ) (cleanup (act (delete-file-or-directory directory #t)))))))
