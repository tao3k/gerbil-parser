;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +fhirpath-syntax-contract+ fhirpath-language-grammar (import: ./grammar)
        fhirpath-language
        parse-fhirpath)

(export +fhirpath-standard-reference+ +fhirpath-standard-version+ +fhirpath-source-uri+ +fhirpath-antlr4-digest+)

(def +fhirpath-standard-reference+
     "HL7 Cross-Paradigm Specification: FHIRPath, Release 1")

(def +fhirpath-standard-version+ "2.0.0")

(def +fhirpath-source-uri+ "https://hl7.org/fhirpath/N1/fhirpath.g4")

(def +fhirpath-antlr4-digest+
     "sha256:cf2a7cf29475e29b1a9188fcabea77782db59c9309b200059b3ef3f781eaae13")

(deflanguage-parser-loader fhirpath-language
  (grammar fhirpath-language-grammar fhirpath-syntax)
  (parse parse-fhirpath)
  (metadata `((language . "fhirpath")
              (version . "2.0.0")
              (contract . "fhirpath-normative-2.0.0-syntax.v1")
              (grammar-format . concise-dsl)
              (source-digest . ,+fhirpath-antlr4-digest+))))

(def +fhirpath-syntax-contract+ (language-metadata-ref (language-parser-entry-ref fhirpath-language 'metadata) 'contract))
