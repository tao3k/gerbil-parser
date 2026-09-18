;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import :gerbil-parser/src/language/entry
        ./grammar)
(export (import: ./grammar)
        fhirpath-v2-language
        parse-fhirpath-v2)

(deflanguage-parser fhirpath-v2-language
  (grammar fhirpath-v2-language-grammar)
  (parse parse-fhirpath-v2))
