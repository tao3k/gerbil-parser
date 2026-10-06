;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        fhirpath-language
        parse-fhirpath)

(deflanguage-parser-loader (fhirpath-language :: self LanguageLoader.)
  (grammar fhirpath-language-grammar)
  (parse parse-fhirpath)
  (slots metadata: (.o grammar-format: 'concise-dsl source-digest: +fhirpath-antlr4-digest+)))
