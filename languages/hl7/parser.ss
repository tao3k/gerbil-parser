;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/projection RecordProjection. deflanguage-projection)
        ./grammar)
(export +hl7-language-version+ +hl7-syntax-contract+ hl7-language-grammar (import: ./grammar)
        hl7-language
        parse-hl7 hl7-adt-a08-patient-projection)

(deflanguage-parser-loader hl7-language
  (grammar hl7-syntax)
  (parse parse-hl7)
  (slots metadata: '((language . "hl7v2")
              (version . "2.5.1")
              (contract . "hl7v2-er7.v1")
              (grammar-format . concise-dsl))))

(def hl7-language-grammar (language-parser-entry-ref hl7-language 'descriptor))

(def +hl7-language-version+ (language-metadata-ref (language-parser-entry-ref hl7-language 'metadata) 'version))

(def +hl7-syntax-contract+ (language-metadata-ref (language-parser-entry-ref hl7-language 'metadata) 'contract))

(deflanguage-projection hl7-adt-a08-patient-projection
  (grammar hl7-syntax)
  (strategy (.o (:: self RecordProjection.) prefix: "MSH" delimiter-count: 5
                expectations: '(((list (split (record "MSH" 8) 1 0) (split (record "MSH" 8) 1 1)) ("ADT" "A08"))
                                ((record "MSH" 11) "2.5.1"))
                constants: '((schema . "gerbil-parser.hl7v2-adt-a08-patient.v1")
                             (sourceInterface . hl7) (sourceMessageType . "ADT^A08") (sourceVersion . "2.5.1"))
                columns: '((identifierSystem concat (text "urn:oid:") (split (split (record "PID" 3) 1 3) 4 1))
                           (identifierValue split (record "PID" 3) 1 0)
                           (familyName split (record "PID" 5) 1 0)
                           (givenNames list (split (record "PID" 5) 1 1))))))
