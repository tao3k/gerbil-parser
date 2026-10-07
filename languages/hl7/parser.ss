;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +hl7-language-version+ +hl7-syntax-contract+ hl7-language-grammar (import: ./grammar)
        hl7-language
        parse-hl7)

(deflanguage-parser-loader hl7-language
  (grammar hl7-language-grammar hl7-syntax)
  (parse parse-hl7)
  (metadata '((language . "hl7v2")
              (version . "2.5.1")
              (contract . "hl7v2-er7.v1")
              (grammar-format . concise-dsl))))

(def +hl7-language-version+ (language-metadata-ref (language-parser-entry-ref hl7-language 'metadata) 'version))

(def +hl7-syntax-contract+ (language-metadata-ref (language-parser-entry-ref hl7-language 'metadata) 'contract))
