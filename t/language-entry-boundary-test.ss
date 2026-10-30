(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Production dispatch and development conformance have distinct dependencies.
(import (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi abi-descriptor-payload abi-parse-binary-payload)
        (only-in :std/vector/u8vector little u8vector-u32-ref)
        (for-syntax (only-in :gerbil/expander core-expand))
        :std/test
        (only-in :clan/poo/object .ref .slot?)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-support/entry LanguageLoaderContract deflanguage-parser-loader)
        (only-in :gerbil-parser/language-support/development
                 LanguageDevelopmentLoaderContract language-loader-fixtures language-loader-fixture-count run-language-test)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source)
        (only-in :gerbil-parser/t/fixtures/language-entries language-entry-pairs)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language)
        (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/gql/parser gql-language)
        (only-in :gerbil-parser/languages/cypher/parser opencypher-language)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-events parse-artifact-roundtrip))

(defsyntax (production-declaration-rejections stx)
  (def (message form)
    (with-catch (lambda (condition) (error-message condition))
      (lambda () (core-expand form) (error "invalid production declaration admitted"))))
  (datum->syntax #'production-declaration-rejections
    (list 'quote
      (list
        (message #'(deflanguage-parser-loader invalid-keyword
                     (grammar #f) (parse invalid-parse) (slots fixtures: '())))
        (message #'(deflanguage-parser-loader invalid-method
                     (grammar #f) (parse invalid-parse) (slots (tests '()))))
        (message #'(deflanguage-parser-loader invalid-metadata
                     (grammar #f) (parse invalid-parse) (metadata '())))
        (message #'(deflanguage-parser-loader invalid-alias
                     (grammar old #f) (parse invalid-parse) (slots metadata: '())))))))
(deflanguage-parser-tests language-entry-boundary-test "production and development entry separation"
  (engine)
    (property "production declarations reject development services and legacy sections before binding"
      (bindings)
      (equal (production-declaration-rejections)
              '("development service requires deflanguage-development-loader"
                  "development service requires deflanguage-development-loader"
                  "unknown language loader section"
                  "invalid language loader declaration")))
    (property "production declarations reject inherited development services"
      (bindings)
      (equal (with-catch (lambda (condition) (error-message condition))
               (lambda ()
                 (let ()
                   (deflanguage-parser-loader (invalid-inherited :: self arithmetic-test-language)
                     (grammar (.ref arithmetic-language 'descriptor)) (parse invalid-parse))
                   invalid-inherited)))
              "production parser entry contains a development service")
      (equal (with-catch (lambda (_) 'rejected)
               (lambda () (run-language-test arithmetic-language 'fixtures) 'admitted))
              'rejected))

    (scenario "native codec uses parser-owned descriptors for accepted and rejected UTF-8 source"
      (each (row) ((list (list gql-language "RETURN '你好'" 0)
              (list opencypher-language "MATCH (n:Person) RETURN n\n" 0)
              (list gql-language "RETURN (" 1) (list opencypher-language "RETURN (" 1)))
        (sequential ((language (bind-language-abi (.ref (car row) 'descriptor))) (source (cadr row)) (expected (caddr row))
               (descriptor (abi-descriptor-payload language))
               (payload (abi-parse-binary-payload language source)))
          (equal (positive? (string-length descriptor))  #t)
          (equal (subu8vector payload 0 4)  #u8(71 80 65 49))
          (equal (u8vector-u32-ref payload 8 little)  expected)
          (equal (u8vector-length payload)
                  (+ 80 (* 24 (u8vector-u32-ref payload 12 little)))))))
    (scenario "all ten production entries need no corpus or development services"
      (equal (length language-entry-pairs)  10)
      (each (row) (language-entry-pairs)
         (act (validate LanguageLoaderContract (car row)))
         (each (name) ('(fixtures fixture-catalog tests scan-workers build-strategies native-test-profile)) (equal (.slot? (car row) name)  #f))))
    (scenario "all development entries preserve the exact production descriptor and corpus"
      (each (row) (language-entry-pairs)
         (scope ((production (car row)) (development (cadr row)))
           (act (validate LanguageDevelopmentLoaderContract development))
           (act (validate LanguageLoaderContract development))
           (equal (eq? (.ref development 'descriptor) (.ref production 'descriptor))  #t)
           (equal (language-loader-fixture-count development)  (caddr row)))))
    (scenario "all accepted and rejected corpora reproduce complete production events"
      (each (row) (language-entry-pairs)
         (each (fixture) ((language-loader-fixtures (cadr row)))
            (sequential ((source (syntax-fixture-source fixture))
                   (production ((.ref (car row) '.parse) source))
                   (development ((.ref (cadr row) '.parse) source)))
              (equal (parse-artifact-success? development)  (parse-artifact-success? production))
              (equal (parse-artifact-events development)  (parse-artifact-events production))
              (equal (parse-artifact-roundtrip development)  (parse-artifact-roundtrip production))
              (equal (parse-artifact-roundtrip production)  source)))))
    (scenario "engine dispatch does not import common adapters or fixture services"
      (scope ((forms (call-with-input-file "src/language/entry.ss"
                    (lambda (port) (read port)))))
        (equal (car forms)  'import)
        (each (item) ((cdr forms))
            (equal (and (pair? item) (eq? (car item) 'only-in)
                        (member (cadr item) '(../../language-support/fixture
                                              ../../language-support/development)))  #f)))))
