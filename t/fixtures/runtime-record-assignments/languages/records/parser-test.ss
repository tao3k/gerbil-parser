;;; -*- Gerbil -*-
;;; Independent downstream conformance uses only installed public facades.
(import :gerbil-parser/language-test-support ./parser)
(deflanguage-parser-tests parser-test "record assignments language pack"
  (loader records-language)
  (identity "immutable public identity"
    (schema "gerbil-parser.language-entry.v2") (language "record-assignments")
    (version "v1") (contract "record-assignments.v1"))
  (fixtures "accepted and rejected declared corpus"))

(def records-parser-tests parser-test)
(export records-parser-tests)
