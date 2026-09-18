#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import :std/test
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-ref parse-artifact-roundtrip
                 parse-artifact-success? parse-artifact-valid?)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        ./parser)

(def grammar-source-path
  "languages/fhirpath/v2.0.0/grammar-source/fhirpath.g4")

(def au-core-fixture-paths
  '("languages/fhirpath/v2.0.0/corpus/au-core-patient/au-core-pat-01.fhirpath"
    "languages/fhirpath/v2.0.0/corpus/au-core-patient/au-core-pat-02.fhirpath"
    "languages/fhirpath/v2.0.0/corpus/au-core-patient/au-core-pat-03.fhirpath"))

(def (read-source path)
  (call-with-input-file path read-all-as-string))

(def fhirpath-v2-parser-test
  (test-suite "FHIRPath 2.0.0 normative syntax"
    (test-case "the official ANTLR grammar identity and bytes are fixed"
      (check +fhirpath-standard-reference+
             => "HL7 Cross-Paradigm Specification: FHIRPath, Release 1")
      (check +fhirpath-standard-version+ => "2.0.0")
      (check +fhirpath-source-uri+
             => "https://hl7.org/fhirpath/N1/fhirpath.g4")
      (check (sha256-text (read-source grammar-source-path))
             => +fhirpath-antlr4-digest+)
      (check +fhirpath-syntax-contract+
             => "fhirpath-normative-2.0.0-syntax.v1"))
    (test-case "AU Core Patient invariant expressions parse losslessly"
      (for-each
       (lambda (path)
         (let* ((source (read-source path))
                (artifact (parse-fhirpath-v2 source)))
           (check (list path (parse-artifact-success? artifact))
                  => (list path #t))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (sha256-text source))))
       au-core-fixture-paths))
    (test-case "normative literal and invocation forms stay lossless"
      (for-each
       (lambda (source)
         (let (artifact (parse-fhirpath-v2 source))
           (check (list source (parse-artifact-success? artifact))
                  => (list source #t))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("Patient.name.where(use = 'official').given"
         "%resource.birthDate >= @2000-01-01"
         "@2015-02-04T14:34:28+09:00 < now()"
         "@T14:34:28.123"
         "4.5 'mg'"
         "text.`div`.exists()"
         "value is FHIR.string")))
    (test-case "an incomplete invocation fails closed"
      (let (artifact (parse-fhirpath-v2 "Patient.name.where("))
        (check (parse-artifact-success? artifact) => #f)))))

(run-tests! fhirpath-v2-parser-test)
