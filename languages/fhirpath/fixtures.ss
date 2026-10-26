;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import 
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.))
(export #t)

(defsyntax-corpus fhirpath-fixtures
  (identity "fhirpath" "2.0.0" +fhirpath-syntax-contract+)
  (accepted
   ("fhirpath/au-core/1" fhirpath-au-core-1 "corpus/au-core-patient/au-core-pat-01.fhirpath" Expression ())
   ("fhirpath/au-core/2" fhirpath-au-core-2 "corpus/au-core-patient/au-core-pat-02.fhirpath" Expression ())
   ("fhirpath/au-core/3" fhirpath-au-core-3 "corpus/au-core-patient/au-core-pat-03.fhirpath" Expression ()))
  (rejected
   ("fhirpath/incomplete" fhirpath-incomplete (text "Patient.name.where("))))

(deflanguage-development-loader fhirpath-test-language
  (grammar fhirpath-language-grammar)
  (parse parse-fhirpath-test)
  (slots metadata: `((grammar-format . concise-dsl) (source-digest . ,+fhirpath-antlr4-digest+))
         fixtures: fhirpath-fixtures))

