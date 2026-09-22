;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/src/language-support/grammar-source
                 defsyntax-antlr4-source))
(export +fhirpath-standard-reference+
        +fhirpath-standard-version+
        +fhirpath-source-uri+
        +fhirpath-antlr4-digest+
        fhirpath-normative-antlr4-source)

(def +fhirpath-standard-reference+
  "HL7 Cross-Paradigm Specification: FHIRPath, Release 1")
(def +fhirpath-standard-version+ "2.0.0")
(def +fhirpath-source-uri+ "https://hl7.org/fhirpath/N1/fhirpath.g4")
(def +fhirpath-antlr4-digest+
  "sha256:cf2a7cf29475e29b1a9188fcabea77782db59c9309b200059b3ef3f781eaae13")

(defsyntax-antlr4-source fhirpath-normative-antlr4-source
  (identity "fhirpath" "2.0.0"
            "https://hl7.org/fhirpath/N1/fhirpath.g4")
  (digest
   "sha256:cf2a7cf29475e29b1a9188fcabea77782db59c9309b200059b3ef3f781eaae13")
  (source "grammar-source/fhirpath.g4"))
