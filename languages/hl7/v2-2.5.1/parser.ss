;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support deflanguage-parser-loader LanguageLoader.)
        ./grammar)
(export (import: ./grammar)
        hl7v2-language
        parse-hl7v2)

(deflanguage-parser-loader (hl7v2-language :: self LanguageLoader.)
  (grammar hl7v2-language-grammar)
  (parse parse-hl7v2)
  (slots metadata: (.o grammar-format: 'concise-dsl)))
