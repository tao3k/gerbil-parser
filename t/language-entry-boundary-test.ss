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
(export language-entry-boundary-test)
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
(def language-entry-boundary-test
  (test-suite "production and development entry separation"
    (test-case "production declarations reject development services and legacy sections before binding"
      (check (production-declaration-rejections)
             => '("development service requires deflanguage-development-loader"
                  "development service requires deflanguage-development-loader"
                  "unknown language loader section"
                  "invalid language loader declaration")))
    (test-case "production declarations reject inherited development services"
      (check (with-catch (lambda (condition) (error-message condition))
               (lambda ()
                 (let ()
                   (deflanguage-parser-loader (invalid-inherited :: self arithmetic-test-language)
                     (grammar (.ref arithmetic-language 'descriptor)) (parse invalid-parse))
                   invalid-inherited)))
             => "production parser entry contains a development service")
      (check (with-catch (lambda (_) 'rejected)
               (lambda () (run-language-test arithmetic-language 'fixtures) 'admitted))
             => 'rejected))

    (test-case "native codec uses parser-owned descriptors for accepted and rejected UTF-8 source"
      (for-each (lambda (row)
        (let* ((language (bind-language-abi (.ref (car row) 'descriptor))) (source (cadr row)) (expected (caddr row))
               (descriptor (abi-descriptor-payload language))
               (payload (abi-parse-binary-payload language source)))
          (check (positive? (string-length descriptor)) => #t)
          (check (subu8vector payload 0 4) => #u8(71 80 65 49))
          (check (u8vector-u32-ref payload 8 little) => expected)
          (check (u8vector-length payload)
                 => (+ 80 (* 24 (u8vector-u32-ref payload 12 little))))))
        (list (list gql-language "RETURN '你好'" 0)
              (list opencypher-language "MATCH (n:Person) RETURN n\n" 0)
              (list gql-language "RETURN (" 1) (list opencypher-language "RETURN (" 1))))
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
