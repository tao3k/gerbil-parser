;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/language-support/grammar deflanguage))
(export hl7-syntax
        hl7-grammar
        hl7-parser-ir
        hl7-parser)


;;; Closed engine rules derive message-local delimiters from MSH-1/MSH-2.
(deflanguage hl7
  (syntax
    (lexical
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
     (extras)
     (keywords)
     (recoveries
      (message "GERBIL-PARSER-HL7V2-ER7" preserve-source))
     (conflicts reject)
     (case-insensitive #f)))
  (rules
    (message
     (node Message
           (seq
            (field header (reference header-segment))
            (repeat (field segment (reference segment))))))
    (header-segment
     (node HeaderSegment
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
     (node Segment
           (seq
            (field name (token segment-id))
            (repeat
             (seq (token field-separator)
                  (optional (field field (reference field)))))
            (token segment-terminator))))
    (field
     (node Field
           (seq
            (field repetition (reference repetition))
            (repeat
             (seq (token repetition-separator)
                  (field repetition (reference repetition)))))))
    (repetition
     (node Repetition
           (seq
            (field component (reference component))
            (repeat
             (seq (token component-separator)
                  (optional (field component (reference component))))))))
    (component
     (node Component
           (seq
            (field subcomponent (reference subcomponent))
            (repeat
             (seq (token subcomponent-separator)
                  (optional
                   (field subcomponent (reference subcomponent))))))))
    (subcomponent
     (node Subcomponent
           (repeat1
            (field value
                   (choice (token data) (token segment-id)
                           (reference escape))))))
    (escape
     (node Escape
           (seq (token escape-character)
                (optional
                 (field value (choice (token data) (token segment-id))))
                (token escape-character))))))
