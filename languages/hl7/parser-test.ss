;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import :gerbil-parser/language-test-support
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/language-support parse-artifact-ref)
        ./parser ./projection)
(deflanguage-parser-tests hl7-parser-test "HL7v2 ER7 2.5.1 parser"
  (loader hl7-language)
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
