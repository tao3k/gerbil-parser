;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage-parser-loader LanguageLoader.
                 defsyntax-corpus check-language-loader-fixtures!)
        ./grammar)
(export (import: ./grammar)
        fhirpath-v2-language
        parse-fhirpath-v2)

(defsyntax-corpus fhirpath-v2-fixtures
  (identity "fhirpath" "2.0.0" +fhirpath-syntax-contract+)
  (accepted
   ("fhirpath/au-core/1" fhirpath-au-core-1 "corpus/au-core-patient/au-core-pat-01.fhirpath" Expression ())
   ("fhirpath/au-core/2" fhirpath-au-core-2 "corpus/au-core-patient/au-core-pat-02.fhirpath" Expression ())
   ("fhirpath/au-core/3" fhirpath-au-core-3 "corpus/au-core-patient/au-core-pat-03.fhirpath" Expression ()))
  (rejected
   ("fhirpath/incomplete" fhirpath-incomplete (text "Patient.name.where("))))

(deflanguage-parser-loader (fhirpath-v2-language :: self LanguageLoader.)
  (grammar fhirpath-v2-language-grammar)
  (parse parse-fhirpath-v2)
  (slots metadata: (.o grammar-format: 'concise-dsl)
         fixtures: (lambda () fhirpath-v2-fixtures)
         tests: (list (cons 'fixtures check-language-loader-fixtures!))))
