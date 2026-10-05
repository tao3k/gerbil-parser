;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/language-support/projection RecordProjection. deflanguage-projection))
(export +hl7-language-version+
        +hl7-syntax-contract+
        hl7-language-grammar
        hl7-grammar
        hl7-parser-ir
        hl7-parser hl7-adt-a08-patient-projection)

(def +hl7-language-version+ "2.5.1")
(def +hl7-syntax-contract+ "hl7v2-er7.v1")

;;; Closed engine rules derive message-local delimiters from MSH-1/MSH-2.
(deflanguage hl7
  (identity "hl7v2" +hl7-language-version+ +hl7-syntax-contract+)
  (root message)
  (lex
   (segment-id SegmentId
    (text-profile
     (seq (run (union (characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (numeric)) 3 3)
          (not-next (union (characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (numeric))))))
   (data Data (header-data "MSH" 5 "\r\n"))
   (segment-terminator SegmentTerminator
    (text-profile
     (if-next (characters "\r")
       (seq (literal "\r") (optional (literal "\n")))
       (literal "\n"))))
   (field-separator FieldSeparator
    (header-delimiter "MSH" 5 0))
   (component-separator ComponentSeparator
    (header-delimiter "MSH" 5 1))
   (repetition-separator RepetitionSeparator
    (header-delimiter "MSH" 5 2))
   (escape-character EscapeCharacter
    (header-delimiter "MSH" 5 3))
   (subcomponent-separator SubcomponentSeparator
    (header-delimiter "MSH" 5 4)))
  (rules
   (message
    (alias Message
      (seq
       (field header (reference header-segment))
       (repeat (field segment (reference segment))))))
   (header-segment
    (alias HeaderSegment
      (seq
       (literal "MSH")
       (token field-separator)
       (token component-separator)
       (token repetition-separator)
       (token escape-character)
       (token subcomponent-separator)
       (repeat
        (seq (token field-separator)
             (optional (field field (reference field)))))
       (token segment-terminator))))
   (segment
    (alias Segment
      (seq
       (field name (token segment-id))
       (repeat
        (seq (token field-separator)
             (optional (field field (reference field)))))
       (token segment-terminator))))
   (field
    (alias Field
      (seq
       (field repetition (reference repetition))
       (repeat
        (seq (token repetition-separator)
             (field repetition (reference repetition)))))))
   (repetition
    (alias Repetition
      (seq
       (field component (reference component))
       (repeat
        (seq (token component-separator)
             (optional (field component (reference component))))))))
   (component
    (alias Component
      (seq
       (field subcomponent (reference subcomponent))
       (repeat
        (seq (token subcomponent-separator)
             (optional
              (field subcomponent (reference subcomponent))))))))
   (subcomponent
    (alias Subcomponent
      (repeat1
       (field value
              (choice (token data) (token segment-id)
                      (reference escape))))))
   (escape
    (alias Escape
      (seq (token escape-character)
           (optional
            (field value (choice (token data) (token segment-id))))
           (token escape-character)))))
  (extras)
  (keywords)
  (recoveries
   (message "GERBIL-PARSER-HL7V2-ER7" preserve-source))
  (conflicts reject)
  (case-insensitive #f)
  (flow
   (source lexical)
   (lexical hl7-er7-structure)
   (hl7-er7-structure cst)))

(deflanguage-projection hl7-adt-a08-patient-projection
  (grammar hl7-language-grammar)
  (strategy (.o (:: self RecordProjection.) prefix: "MSH" delimiter-count: 5
    expectations: '(((list (split (record "MSH" 8) 1 0) (split (record "MSH" 8) 1 1)) ("ADT" "A08"))
                    ((record "MSH" 11) "2.5.1"))
    constants: '((schema . "gerbil-parser.hl7v2-adt-a08-patient.v1")
                 (sourceInterface . hl7) (sourceMessageType . "ADT^A08") (sourceVersion . "2.5.1"))
    columns: '((identifierSystem concat (text "urn:oid:") (split (split (record "PID" 3) 1 3) 4 1))
               (identifierValue split (record "PID" 3) 1 0)
               (familyName split (record "PID" 5) 1 0)
               (givenNames list (split (record "PID" 5) 1 1))))))
