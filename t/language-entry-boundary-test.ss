;;; -*- Gerbil -*-
;;; Production dispatch and development conformance have distinct dependencies.
(import :std/test
        (only-in :clan/poo/object .ref .slot?)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-support/entry LanguageLoaderContract)
        (only-in :gerbil-parser/language-support/development
                 LanguageDevelopmentLoaderContract language-loader-fixture-count)
        (only-in :gerbil-parser/languages/gql/parser gql-language parse-gql +gql-representative-query+)
        (only-in :gerbil-parser/languages/gql/parser-test gql-test-language)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-roundtrip))
(export language-entry-boundary-test)
(def language-entry-boundary-test
  (test-suite "production and development entry separation"
    (test-case "production entry needs no corpus or development services"
      (validate LanguageLoaderContract gql-language)
      (for-each (lambda (name) (check (.slot? gql-language name) => #f))
                '(fixtures fixture-catalog tests scan-workers build-strategies)))
    (test-case "development entry preserves the exact production descriptor"
      (validate LanguageDevelopmentLoaderContract gql-test-language)
      (validate LanguageLoaderContract gql-test-language)
      (check (eq? (.ref gql-test-language 'descriptor) (.ref gql-language 'descriptor)) => #t)
      (check (language-loader-fixture-count gql-test-language) => 14))
    (test-case "development and production dispatch reproduce complete public events"
      (let ((production (parse-gql +gql-representative-query+))
            (development ((.ref gql-test-language '.parse) +gql-representative-query+)))
        (check (parse-artifact-events development) => (parse-artifact-events production))
        (check (parse-artifact-roundtrip production) => +gql-representative-query+)))
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
