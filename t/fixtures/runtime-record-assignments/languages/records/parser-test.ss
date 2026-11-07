;;; -*- Gerbil -*-
;;; Independent downstream conformance uses only installed public facades.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader)
        (only-in :gerbil-parser/language-support/entry language-parser-entry-ref)
        ./parser ./fixtures)
(deflanguage-development-loader records-test-language
  (descriptor (language-parser-entry-ref records-language 'descriptor))
  (parse parse-records-test)
  (slots fixtures: records-fixtures))
(deflanguage-parser-tests parser-test "record assignments language pack"
  (loader records-test-language)
  (identity "immutable public identity"
    (schema "gerbil-parser.language-entry.v2") (language "record-assignments")
    (version "v1") (contract "record-assignments.v1"))
  (fixtures "accepted and rejected declared corpus"))

(def records-parser-tests parser-test)
(export records-parser-tests records-test-language)
