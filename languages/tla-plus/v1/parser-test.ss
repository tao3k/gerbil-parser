#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for pinned TLA+ sources, structural CST obligations,
;;; nested-comment losslessness, and typed unterminated-comment failure.

(import (only-in :std/test check test-case test-suite)
        (only-in :std/misc/process run-process)
        (only-in :std/srfi/13 string-trim-right)
        :gerbil-parser/languages/tla-plus/v1/parser
        (only-in :gerbil-parser/languages/tla-plus/v1/qualification
                 +tla-plus-model-qualification-schema+
                 qualify-tla-plus-model
                 tla-plus-model-receipt-admitted
                 tla-plus-model-receipt->alist)
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/language-support
                 syntax-fixture-required-kinds
                 syntax-fixture-source
                 syntax-fixture-source-digest
                 syntax-fixture-expected-status)
        (only-in ./fixtures tla-plus-v1-fixtures
                 tla-plus-v1-accepted-fixtures
                 tla-plus-v1-rejected-fixtures))
(export tla-plus-v1-parser-test)

(def (receipt-ref receipt key)
  (let (entry (assq key (tla-plus-model-receipt->alist receipt)))
    (and entry (cdr entry))))

(def (call-with-qualification-fixture procedure)
  (let* ((template
          (path-expand "gerbil-parser-tlc-test.XXXXXX"
                       (getenv "TMPDIR" "/tmp")))
         (directory
          (string-trim-right (run-process ["mktemp" "-d" template]))))
    (unwind-protect
      (procedure directory)
      (when (file-exists? directory)
        (delete-file-or-directory directory #t)))))

;;; CST traversal intentionally treats fields as transparent containers; the
;;; observable contract is the ordered set of emitted syntax-node kinds.
;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;; : (-> ParseArtifact Symbol Integer)
(def (artifact-token-count artifact kind)
  (length
   (filter (lambda (event)
             (and (token-event? event)
                  (eq? (token-event-token-kind event) kind)))
           (parse-artifact-events artifact))))

;; : TestSuite
(def tla-plus-v1-parser-test
  (test-suite "TLA+ v1 versioned language pack"
    (test-case "native syntax and corpus identities are immutable"
      (check +tla-plus-contract-version+ => "v1")
      (check +tla-plus-syntax-contract+ => "tla-plus.native-core.v1")
      (check +tla-plus-syntax-source+
             => "Specifying Systems, Chapter 15: TLAPlusGrammar")
      (check +tla-plus-sany-release+ => "v1.7.4")
      (check +tla-plus-sany-commit+
             => "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
      (check +tla-plus-sany-grammar-blob+
             => "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
      (check +tla-plus-sany-grammar-digest+
             => "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")
      (check +tla-plus-examples-commit+
             => "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
      (check (length tla-plus-v1-fixtures) => 6)
      (check (length tla-plus-v1-accepted-fixtures) => 5)
      (check (length tla-plus-v1-rejected-fixtures) => 1)
      (check (syntax-fixture-source-digest (car tla-plus-v1-fixtures))
             => "sha256:985903176db4725f9cf25df84ad84dcd53ba94be80d26298b87ec86fbc9b08b3"))
    (test-case "all admitted modules publish lossless structural CSTs"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-tla-plus-v1 source))
                (root (and (parse-artifact-success? artifact)
                           (parse-artifact->cst artifact)))
                (kinds (and root (cst-node-kinds root))))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (syntax-node-kind root) => 'SourceFile)
           (for-each
            (lambda (kind) (check (member kind kinds) ? values))
            (syntax-fixture-required-kinds fixture))))
       tla-plus-v1-accepted-fixtures))
    (test-case "nested block comments remain one lossless trivia token"
      (let* ((fixture (caddr tla-plus-v1-accepted-fixtures))
             (artifact (parse-tla-plus-v1
                        (syntax-fixture-source fixture))))
        (check (parse-artifact-success? artifact) => #t)
        (check (artifact-token-count artifact 'comment) => 1)))
    (test-case "unary negation is admitted by the native lexical contract"
      (let* ((source
              "---- MODULE Negation ----\nVARIABLES enabled, ready\nDisabled == ~enabled /\\ ready\nNext == enabled' = FALSE /\\ ready' = TRUE\n====\n")
             (artifact (parse-tla-plus-v1 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "one qualification API composes parser and TLC receipts"
      (call-with-qualification-fixture
       (lambda (directory)
         (let ((spec (path-expand "Qualified.tla" directory))
               (config (path-expand "Qualified.cfg" directory))
               (tlc (path-expand "tlc-fixture" directory)))
           (call-with-output-file
            spec
            (lambda (port)
              (display
               "---- MODULE Qualified ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n"
               port)))
           (call-with-output-file
            config
            (lambda (port) (display "INIT Init\nNEXT Next\n" port)))
           (call-with-output-file
            tlc
            (lambda (port)
              (display
               "#!/bin/sh\ncat <<'EOF'\nTLC2 Version fixture\nModel checking completed. No error has been found.\n4 states generated, 2 distinct states found, 0 states left on queue.\nThe depth of the complete state graph search is 1.\nEOF\n"
               port)))
           (run-process ["chmod" "+x" tlc])
           (let (receipt
                 (qualify-tla-plus-model spec config tlc: tlc workers: 1))
             (check +tla-plus-model-qualification-schema+
                    => "gerbil-parser.tla-plus-model-qualification.v1")
             (check (tla-plus-model-receipt-admitted receipt) => #t)
             (check (receipt-ref receipt 'syntax-accepted) => #t)
             (check (receipt-ref receipt 'roundtrip) => #t)
             (check (receipt-ref receipt 'states-generated) => 4)
             (check (receipt-ref receipt 'distinct-states) => 2)
             (check (receipt-ref receipt 'states-left) => 0)
             (check (receipt-ref receipt 'graph-depth) => 1))))))
    (test-case "qualification fails closed on misleading TLC completion"
      (call-with-qualification-fixture
       (lambda (directory)
         (let ((spec (path-expand "Rejected.tla" directory))
               (config (path-expand "Rejected.cfg" directory))
               (tlc (path-expand "tlc-fixture" directory)))
           (call-with-output-file
            spec
            (lambda (port)
              (display
               "---- MODULE Rejected ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n"
               port)))
           (call-with-output-file
            config
            (lambda (port) (display "INIT Init\nNEXT Next\n" port)))
           (call-with-output-file
            tlc
            (lambda (port)
              (display
               "#!/bin/sh\necho 'TLC2 Version fixture'\necho 'Model checking completed. No error has been found.'\necho '4 states generated, 2 distinct states found, 0 states left on queue.'\necho 'The depth of the complete state graph search is 1.'\nexit 7\n"
               port)))
           (run-process ["chmod" "+x" tlc])
           (let (receipt
                 (qualify-tla-plus-model spec config tlc: tlc workers: 1))
             (check (tla-plus-model-receipt-admitted receipt) => #f)
             (check (zero? (receipt-ref receipt 'exit-status)) => #f))
           (call-with-output-file
            tlc
            (lambda (port)
              (display
               "#!/bin/sh\necho 'TLC2 Version fixture'\necho 'Model checking completed. No error has been found.'\n"
               port)))
           (let (receipt
                 (qualify-tla-plus-model spec config tlc: tlc workers: 1))
             (check (tla-plus-model-receipt-admitted receipt) => #f)
             (check (receipt-ref receipt 'states-generated) => #f))))))
    (test-case "syntax rejection stops before resolving TLC"
      (call-with-qualification-fixture
       (lambda (directory)
         (let ((spec (path-expand "Malformed.tla" directory))
               (config (path-expand "Malformed.cfg" directory)))
           (call-with-output-file
            spec
            (lambda (port)
              (display
               "---- MODULE Malformed ----\nVARIABLE x\nBroken == IF x = 0 THEN ELSE x\n====\n"
               port)))
           (call-with-output-file
            config
            (lambda (port) (display "INIT Broken\n" port)))
           (let (receipt
                 (qualify-tla-plus-model
                  spec config tlc: "this-tlc-must-not-be-resolved"))
             (check (tla-plus-model-receipt-admitted receipt) => #f)
             (check (receipt-ref receipt 'syntax-accepted) => #f)
             (check (receipt-ref receipt 'tool-path) => #f)
             (check (receipt-ref receipt 'exit-status) => #f))))))
    (test-case "unterminated nested comments fail as one typed artifact"
      (let (artifact
            (parse-tla-plus-v1
             "---- MODULE Broken ----\n(* outer (* nested *)\nVARIABLE x\n====\n"))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
    (test-case "recognized but malformed expressions fail closed"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-tla-plus-v1 source)))
           (check (syntax-fixture-expected-status fixture) => 'rejected)
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       tla-plus-v1-rejected-fixtures))))
