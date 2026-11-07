;;; -*- Gerbil -*-
;;; Native ABI fixture publication has a build owner separate from test assertions.
(import (only-in :std/encoding/hex hex-encode)
        (only-in :std/encoding/json write-json current-json-write-options JSONWriteOptions)
        (prefix-in (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi abi-descriptor-payload abi-parse-binary-payload) codec-)
        (only-in :gerbil-parser/language-support/entry language-metadata-ref)
        (only-in :clan/poo/object .ref)
        (only-in :gerbil-parser/t/fixtures/language-entries language-entry-pairs)
        (only-in :gerbil-parser/language-support/development language-loader-fixtures)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success?))
(export write-language-abi-alignment! write-language-abi-fixtures! report-language-abi-mismatch!)
;;; Each native backend is checked from its configured public parser descriptor.
(def (for-each-language-abi-case procedure)
  (for-each
    (lambda (row)
      (let* ((entry (car row)) (development (cadr row))
             (metadata (.ref entry 'metadata))
             (name (language-metadata-ref metadata 'contract))
             (native (codec-bind-language-abi (.ref entry 'descriptor))))
        (for-each
          (lambda (status)
            (let (fixtures (language-loader-fixtures development status))
              (when (null? fixtures) (error "missing native alignment control" name status))
              (let* ((source (syntax-fixture-source (car fixtures)))
                     (artifact ((.ref entry '.parse) source))
                     (accepted? (eq? status 'accepted)))
                (unless (eq? (parse-artifact-success? artifact) accepted?)
                  (error "native alignment fixture status mismatch" name status))
                (procedure name (codec-abi-descriptor-payload native) source
                           (codec-abi-parse-binary-payload native source) accepted?)
                (displayln "NATIVE-ALIGNMENT-CASE " name " " status) (force-output))))
          '(accepted rejected))))
    language-entry-pairs))

(def (write-language-abi-alignment! output)
  (def (emit-bytes port bytes)
    (let (length (u8vector-length bytes))
      (for-each (lambda (shift) (write-u8 (bitwise-and 255 (arithmetic-shift length (- shift))) port))
                '(0 8 16 24)))
    (write-u8vector bytes port))
  (call-with-output-file output
    (lambda (port)
      (for-each-language-abi-case
        (lambda (name descriptor source payload accepted?)
          (for-each (lambda (bytes) (emit-bytes port bytes))
                    (list (string->utf8 name) (string->utf8 descriptor)
                          (string->utf8 source) payload))
          (write-u8 (if accepted? 1 0) port)))))
  (displayln "LANGUAGE-ABI-ALIGNMENT-GENERATED") (force-output))

;; Text fixtures use the SDK JSON encoder and the same canonical case producer.
(def (write-language-abi-fixtures! output)
  (let (rows '())
    (for-each-language-abi-case
      (lambda (name descriptor source payload accepted?)
        (let (row (make-hash-table))
          (for-each (lambda (entry) (hash-put! row (car entry) (cdr entry)))
                    (list (cons "name" name) (cons "descriptor" descriptor)
                          (cons "source" source) (cons "accepted" accepted?)
                          (cons "payload_hex" (hex-encode payload))))
          (set! rows (cons row rows)))))
    (call-with-output-file output
      (lambda (port)
        (parameterize ((current-json-write-options
                         (JSONWriteOptions sort-keys: #t pretty: 'standard indent: 2)))
          (write-json port (list->vector (reverse rows)))))))
  (displayln "LANGUAGE-ABI-FIXTURES-GENERATED") (force-output))

;; Diagnostic output belongs to this fixture owner, not a -test.ss assertion.
;; Keep exact-byte admission in the caller and report only a bounded window.
(def (report-language-abi-mismatch! actual expected)
  (let (extent (min (u8vector-length actual) (u8vector-length expected)))
    (let find ((at 0))
      (if (and (< at extent) (= (u8vector-ref actual at) (u8vector-ref expected at)))
        (find (+ at 1))
        (begin
          (displayln "ABI-BYTE-MISMATCH offset=" at
            " actual-bytes=" (u8vector-length actual)
            " expected-bytes=" (u8vector-length expected))
          (displayln "ABI-ACTUAL " (hex-encode actual (max 0 (- at 32))
                                    (min (u8vector-length actual) (+ at 64))))
          (displayln "ABI-EXPECTED " (hex-encode expected (max 0 (- at 32))
                                      (min (u8vector-length expected) (+ at 64))))
          (force-output))))))
