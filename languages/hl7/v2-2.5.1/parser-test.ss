;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import :std/test
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-ref parse-artifact-roundtrip
                 parse-artifact-success? parse-artifact-valid?)
        ./parser
        ./projection)

(def fixture-path "languages/hl7/v2-2.5.1/corpus/adt-a08-patient.hl7")

(def hl7v2-parser-test
  (test-suite "HL7v2 ER7 2.5.1 parser"
    (test-case "ADT A08 is accepted, lossless and projected with receipt identity"
      (let* ((source (call-with-input-file fixture-path read-all-as-string))
             (artifact (parse-hl7v2 source))
             (projection (hl7v2-adt-a08-patient-projection artifact)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (cdr (assq 'sourceDigest projection))
               => (parse-artifact-ref artifact 'sourceDigest))
        (check (cdr (assq 'sourceMessageType projection)) => "ADT^A08")
        (check (cdr (assq 'identifierValue projection)) => "8003608166690503")
        (check (cdr (assq 'familyName projection)) => "Nguyen")
        (check (cdr (assq 'givenNames projection)) => '("Ava"))))
    (test-case "message-local delimiter declaration is parsed losslessly"
      (let* ((source
              "MSH*$%!?*LEGACY*AU*FHIR*AU*202609170900**ADT$A08*1*P*2.5.1\r")
             (artifact (parse-hl7v2 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))))

(export hl7v2-parser-test)
