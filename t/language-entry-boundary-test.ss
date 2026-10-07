;;; -*- Gerbil -*-
;;; Production dispatch and development conformance have distinct dependencies.
(import (prefix-in (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-native-language native-descriptor-payload native-parse-binary-payload) codec-)
        (only-in :gerbil-parser/language-support/entry language-metadata-ref)
        (only-in :gerbil-parser/src/ffi/parse-artifact-v1 native-descriptor-payload native-parse-binary-payload)
        (only-in :std/vector/u8vector little u8vector-u32-ref)
        :std/test
        (only-in :clan/poo/object .ref .slot?)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-support/entry LanguageLoaderContract)
        (only-in :gerbil-parser/language-support/development
                 LanguageDevelopmentLoaderContract language-loader-fixtures language-loader-fixture-count)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language)
        (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/bash/parser bash-language)
        (only-in :gerbil-parser/languages/bash/parser-test bash-test-language)
        (only-in :gerbil-parser/languages/cypher/parser opencypher-language)
        (only-in :gerbil-parser/languages/cypher/parser-test opencypher-test-language)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-language)
        (only-in :gerbil-parser/languages/fhirpath/parser-test fhirpath-test-language)
        (only-in :gerbil-parser/languages/gql/parser gql-language)
        (only-in :gerbil-parser/languages/gql/parser-test gql-test-language)
        (only-in :gerbil-parser/languages/hcl/parser hcl-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/hl7/parser hl7-language)
        (only-in :gerbil-parser/languages/hl7/parser-test hl7-test-language)
        (only-in :gerbil-parser/languages/tla-plus/parser
                 tla-plus-core-language tla-plus-layout-language tla-plus-sany-candidate-language)
        (only-in :gerbil-parser/languages/tla-plus/parser-test
                 tla-plus-core-test-language tla-plus-layout-test-language tla-plus-sany-candidate-test-language)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-events parse-artifact-roundtrip))
(export language-entry-boundary-test write-native-language-alignment!)
(def language-entry-pairs
  (list (list arithmetic-language arithmetic-test-language 2)
        (list bash-language bash-test-language 2)
        (list opencypher-language opencypher-test-language 5)
        (list fhirpath-language fhirpath-test-language 4)
        (list gql-language gql-test-language 15)
        (list hcl-language hcl-test-language 15)
        (list hl7-language hl7-test-language 2)
        (list tla-plus-core-language tla-plus-core-test-language 6)
        (list tla-plus-layout-language tla-plus-layout-test-language 2)
        (list tla-plus-sany-candidate-language tla-plus-sany-candidate-test-language 2)))
(def language-entry-boundary-test
  (test-suite "production and development entry separation"
    (test-case "native facade uses parser-owned descriptors for accepted and rejected UTF-8 source"
      (for-each (lambda (row)
        (let* ((language (car row)) (source (cadr row)) (expected (caddr row))
               (descriptor (native-descriptor-payload language))
               (payload (native-parse-binary-payload language source)))
          (check (positive? (string-length descriptor)) => #t)
          (check (subu8vector payload 0 4) => #u8(71 80 65 49))
          (check (u8vector-u32-ref payload 8 little) => expected)
          (check (u8vector-length payload)
                 => (+ 80 (* 24 (u8vector-u32-ref payload 12 little))))))
        (quote (("gql" "RETURN '你好'" 0)
                ("cypher" "MATCH (n:Person) RETURN n\n" 0)
                ("gql" "RETURN (" 1) ("cypher" "RETURN (" 1)))))
    (test-case "all ten production entries need no corpus or development services"
      (check (length language-entry-pairs) => 10)
      (for-each
       (lambda (row)
         (validate LanguageLoaderContract (car row))
         (for-each (lambda (name) (check (.slot? (car row) name) => #f))
                   '(fixtures fixture-catalog tests scan-workers build-strategies native-test-profile)))
       language-entry-pairs))
    (test-case "all development entries preserve the exact production descriptor and corpus"
      (for-each
       (lambda (row)
         (let ((production (car row)) (development (cadr row)))
           (validate LanguageDevelopmentLoaderContract development)
           (validate LanguageLoaderContract development)
           (check (eq? (.ref development 'descriptor) (.ref production 'descriptor)) => #t)
           (check (language-loader-fixture-count development) => (caddr row))))
       language-entry-pairs))
    (test-case "all accepted and rejected corpora reproduce complete production events"
      (for-each
       (lambda (row)
         (for-each
          (lambda (fixture)
            (let* ((source (syntax-fixture-source fixture))
                   (production ((.ref (car row) '.parse) source))
                   (development ((.ref (cadr row) '.parse) source)))
              (check (parse-artifact-success? development) => (parse-artifact-success? production))
              (check (parse-artifact-events development) => (parse-artifact-events production))
              (check (parse-artifact-roundtrip development) => (parse-artifact-roundtrip production))
              (check (parse-artifact-roundtrip production) => source)))
          (language-loader-fixtures (cadr row))))
       language-entry-pairs))
    (test-case "engine dispatch does not import common adapters or fixture services"
      (let (forms (call-with-input-file "src/language/entry.ss"
                    (lambda (port) (read port))))
        (check (car forms) => 'import)
        (for-each
          (lambda (item)
            (check (and (pair? item) (eq? (car item) 'only-in)
                        (member (cadr item) '(../../language-support/fixture
                                              ../../language-support/development))) => #f))
          (cdr forms))))))

;;; Each native backend is checked from its configured public parser descriptor.
(def (write-native-language-alignment! output)
  (def (emit-bytes port bytes)
    (let (length (u8vector-length bytes))
      (for-each (lambda (shift) (write-u8 (bitwise-and 255 (arithmetic-shift length (- shift))) port))
                '(0 8 16 24)))
    (write-u8vector bytes port))
  (call-with-output-file output
    (lambda (port)
      (for-each
        (lambda (row)
          (let* ((entry (car row)) (development (cadr row))
                 (metadata (.ref entry 'metadata))
                 (name (language-metadata-ref metadata 'contract))
                 (native (codec-bind-native-language (.ref entry 'descriptor))))
            (for-each
              (lambda (status)
                (let (fixtures (language-loader-fixtures development status))
                  (when (null? fixtures) (error "missing native alignment control" name status))
                  (let* ((source (syntax-fixture-source (car fixtures)))
                         (artifact ((.ref entry '.parse) source)))
                    (unless (eq? (parse-artifact-success? artifact) (eq? status 'accepted))
                      (error "native alignment fixture status mismatch" name status))
                    (emit-bytes port (string->utf8 name))
                    (emit-bytes port (string->utf8 (codec-native-descriptor-payload native)))
                    (emit-bytes port (string->utf8 source))
                    (emit-bytes port (codec-native-parse-binary-payload native source))
                    (write-u8 (if (eq? status 'accepted) 1 0) port)
                    (displayln "NATIVE-ALIGNMENT-CASE " name " " status) (force-output))))
              '(accepted rejected))))
        language-entry-pairs)))
  (displayln "NATIVE-LANGUAGE-ALIGNMENT-GENERATED") (force-output))
