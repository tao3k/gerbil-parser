;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        hl7-language
        parse-hl7)

(deflanguage-parser-loader (hl7-language :: self LanguageLoader.)
  (grammar hl7-language-grammar)
  (parse parse-hl7)
  (slots metadata: (.o grammar-format: 'concise-dsl)))
