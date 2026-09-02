;;; -*- Gerbil -*-
;;; Arithmetic language-pack entry and native fixture contract.

(import :std/test
        :gerbil-parser/src/language/entry
        :gerbil-parser/src/runtime/artifact
        ../../support/fixture
        ./fixtures
        ./parser)
(export arithmetic-v1-parser-test)

(def arithmetic-v1-parser-test
  (test-suite "arithmetic v1 language pack"
    (test-case "the declarative entry parses its colocated native fixture"
      (let* ((fixture arithmetic-v1-basic-fixture)
             (source (syntax-fixture-source fixture))
             (artifact (parse-arithmetic-v1 source)))
        (check (language-parser-entry-ref arithmetic-v1-language 'schema)
               => +language-parser-entry-schema-v1+)
        (check (language-parser-entry-ref arithmetic-v1-language 'language)
               => "arithmetic")
        (check (language-parser-entry-ref arithmetic-v1-language 'version)
               => "v1")
        (check (language-parser-entry-ref arithmetic-v1-language 'contract)
               => "arithmetic-expression.v1")
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-ref artifact 'sourceDigest)
               => (syntax-fixture-source-digest fixture))
        (check (parse-artifact-roundtrip artifact) => source)))))
