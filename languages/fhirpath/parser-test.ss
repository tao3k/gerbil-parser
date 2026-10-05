;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import :gerbil-parser/language-test-support
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        ./parser)
(deflanguage-parser-tests fhirpath-parser-test "FHIRPath 2.0.0 normative syntax"
  (loader fhirpath-language)
  (property "official ANTLR grammar identity and bytes"
    (bindings)
    (equal +fhirpath-standard-reference+ "HL7 Cross-Paradigm Specification: FHIRPath, Release 1")
    (equal +fhirpath-standard-version+ "2.0.0")
    (equal +fhirpath-source-uri+ "https://hl7.org/fhirpath/N1/fhirpath.g4")
    (equal (sha256-text (call-with-input-file "languages/fhirpath/grammar-source/fhirpath.g4" read-all-as-string))
           +fhirpath-antlr4-digest+)
    (equal +fhirpath-syntax-contract+ "fhirpath-normative-2.0.0-syntax.v1"))
  (fixtures "AU Core Patient loader fixtures parse losslessly")
  (accepted-many "normative literal and invocation forms"
    '("Patient.name.where(use = 'official').given"
      "%resource.birthDate >= @2000-01-01"
      "@2015-02-04T14:34:28+09:00 < now()" "@T14:34:28.123"
      "4.5 'mg'" "text.`div`.exists()" "value is FHIR.string"))
  (accepted-many "strict string and delimited identifier profiles"
    '("'α'" "'\\u0041'" "`a\\`b`"))
  (rejected-many "malformed string and identifier escapes"
    '("'\\q'" "'\\u123'" "`a\\x`"))
  (rejected "incomplete invocation fails closed" "Patient.name.where("))
