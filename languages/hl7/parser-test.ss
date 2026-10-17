;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import :gerbil-parser/language-test-support
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/language-support parse-artifact-ref)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.))
(export hl7-fixtures hl7-test-language)

(defsyntax-corpus hl7-fixtures
  (identity "hl7v2" +hl7-language-version+ +hl7-syntax-contract+)
  (accepted
   ("hl7v2/adt-a08" hl7-adt-a08 "corpus/adt-a08-patient.hl7" Message (HeaderSegment Segment Field)))
  (rejected
   ("hl7v2/missing-header" hl7-missing-header (text "PID|1\r"))))

(deflanguage-development-loader hl7-test-language
  (grammar hl7-language-grammar)
  (parse parse-hl7-test)
  (slots metadata: '((grammar-format . concise-dsl))
         fixtures: hl7-fixtures))

(deflanguage-parser-tests hl7-parser-test "HL7v2 ER7 2.5.1 parser"
  (loader hl7-test-language)
  (accepted "ADT A08 is lossless and projected"
    (call-with-input-file "languages/hl7/corpus/adt-a08-patient.hl7" read-all-as-string)
    (projection hl7-adt-a08-patient-projection
      (sourceMessageType "ADT^A08") (identifierValue "8003608166690503")
      (familyName "Nguyen") (givenNames '("Ava"))))
  (property "projection source identity matches its artifact"
    (bindings (artifact (parse-hl7 (call-with-input-file
                                     "languages/hl7/corpus/adt-a08-patient.hl7" read-all-as-string)))
              (projection (hl7-adt-a08-patient-projection artifact)))
    (equal (cdr (assq 'sourceDigest projection))
           (parse-artifact-ref artifact 'sourceDigest)))
  (rejected-many "missing delimiter headers retain valid lossless artifacts"
    '("PID|1\r" "PID|α\r"))
  (accepted "message-local delimiters"
    "MSH*$%!?*LEGACY*AU*FHIR*AU*202609170900**ADT$A08*1*P*2.5.1\r")
  (fixtures "loader fixture conformance"))
