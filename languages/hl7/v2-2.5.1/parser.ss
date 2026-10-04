;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage-parser-loader LanguageLoader.
                 defsyntax-corpus check-language-loader-fixtures!)
        ./grammar)
(export (import: ./grammar)
        hl7v2-language
        parse-hl7v2)

(defsyntax-corpus hl7v2-fixtures
  (identity "hl7v2" +hl7v2-language-version+ +hl7v2-syntax-contract+)
  (accepted
   ("hl7v2/adt-a08" hl7v2-adt-a08 "corpus/adt-a08-patient.hl7" Message (HeaderSegment Segment Field)))
  (rejected
   ("hl7v2/missing-header" hl7v2-missing-header (text "PID|1\r"))))

(deflanguage-parser-loader (hl7v2-language :: self LanguageLoader.)
  (grammar hl7v2-language-grammar)
  (parse parse-hl7v2)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () hl7v2-fixtures)
         tests: (list (cons 'fixtures check-language-loader-fixtures!))))
