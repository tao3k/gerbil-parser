;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Shared corpus and development loader declarations; no Suite execution.
(import ./parser
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.))
(export #t)

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
