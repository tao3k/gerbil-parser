;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/language-support deflanguage)
        ./scanner)
(export +hl7v2-language-version+
        +hl7v2-syntax-contract+
        hl7v2-language-grammar
        hl7v2-grammar
        hl7v2-parser-ir
        hl7v2-parser)

(def +hl7v2-language-version+ "2.5.1")
(def +hl7v2-syntax-contract+ "hl7v2-er7.v1")

;;; Every delimiter scanner derives its character from MSH-1/MSH-2, preserving
;;; ER7's message-local delimiter contract without mutable parser state.
(deflanguage hl7v2
  (identity "hl7v2" +hl7v2-language-version+ +hl7v2-syntax-contract+)
  (root message)
  (lex
   (segment-id SegmentId (external hl7v2-segment-id-v1 scan-hl7v2-segment-id))
   (data Data (external hl7v2-data-v1 scan-hl7v2-data))
   (segment-terminator SegmentTerminator
    (external hl7v2-segment-terminator-v1
              scan-hl7v2-segment-terminator))
   (field-separator FieldSeparator
    (external hl7v2-field-separator-v1 scan-hl7v2-field-separator))
   (component-separator ComponentSeparator
    (external hl7v2-component-separator-v1 scan-hl7v2-component-separator))
   (repetition-separator RepetitionSeparator
    (external hl7v2-repetition-separator-v1 scan-hl7v2-repetition-separator))
   (escape-character EscapeCharacter
    (external hl7v2-escape-character-v1 scan-hl7v2-escape-character))
   (subcomponent-separator SubcomponentSeparator
    (external hl7v2-subcomponent-separator-v1
              scan-hl7v2-subcomponent-separator)))
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
   (lexical hl7v2-er7-structure)
   (hl7v2-er7-structure cst)))
