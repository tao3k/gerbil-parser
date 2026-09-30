#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Acceptance owner for the pinned HCL specsuite, descriptor identity,
;;; lossless accepted artifacts, and typed rejected artifacts.

(import (only-in :std/test check test-case test-suite)
        :gerbil-parser/languages/hcl/v2-24/parser
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-runtime parser-machine-trivia
                 parser-machine-grammar-digest
                 parser-machine-direct-source)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-initial-checkpoint lr-checkpoint-drive
                 lr-runtime-direct-step)
        (only-in :gerbil-parser/src/runtime/lexer
                 lex-source scan-source-token)
        (only-in :gerbil-parser/src/runtime/token token-end)
        :gerbil-parser/src/language/entry
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/language-support
                 syntax-fixture-contract
                 syntax-fixture-expected-status
                 syntax-fixture-id
                 syntax-fixture-required-kinds
                 syntax-fixture-root-kind
                 syntax-fixture-source
                 syntax-fixture-source-digest
                 syntax-fixture-version)
        (only-in ./fixtures
                 hcl-v2-24-official-accepted-fixtures
                 hcl-v2-24-official-fixtures
                 hcl-v2-24-official-rejected-fixtures)
        (only-in ./direct-recursive direct-parse-hcl direct-lex-hcl))
(export hcl-v2-24-parser-test)

;;; Structural traversal stays independent of HCL production nesting so the
;;; official corpus checks semantic kinds while roundtrip checks retain bytes.
;; : (-> CSTValue (List Symbol))
(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

;;; The A/B override exercises the same scanner and materialization path while
;;; routing only deterministic reductions through the indexed baseline.
(def (parse-hcl-indexed-baseline source)
  (let ((machine hcl-v2-24-parser)
        (character-offset 0)
        (byte-offset 0)
        (pending-character #f)
        (tokens-reversed '()))
    (def (next-input mode)
      (if (= character-offset (string-length source))
        #f
        (let-values (((input-token next-character)
                      (scan-source-token machine source
                                         character-offset byte-offset mode)))
          (let (start-character character-offset)
            (set! character-offset next-character)
            (set! byte-offset (token-end input-token))
            (if ((parser-machine-trivia machine) input-token)
              (begin
                (set! tokens-reversed (cons input-token tokens-reversed))
                (next-input mode))
              (begin
                (set! pending-character start-character)
                input-token))))))
    (def (after-shift input-token _states _values _actions _shifts)
      (set! tokens-reversed (cons input-token tokens-reversed))
      (set! pending-character #f))
    (let-values (((status payload)
                  (lr-checkpoint-drive
                   (lr-initial-checkpoint
                    (parser-machine-runtime machine) '())
                   next-input after-shift #f #f)))
      (unless (and (eq? status 'accepted)
                   (= character-offset (string-length source))
                   (not pending-character)
                   (null? (cadr payload)))
        (error "HCL indexed baseline did not complete" status))
      (make-success-parse-artifact
       (parser-machine-grammar-digest machine)
       source (reverse tokens-reversed)
       (car payload) (parser-machine-trivia machine)))))

;; : TestSuite
(def hcl-v2-24-parser-test
  (test-suite "HCL native syntax v2.24.0 official corpus"
    (test-case "HCL machine installs the generated fused step"
      (check (procedure?
              (lr-runtime-direct-step
               (parser-machine-runtime hcl-v2-24-parser))) => #t))
    (test-case "HCL machine installs the Grammar IR recursive source parser"
      (check (procedure? (parser-machine-direct-source hcl-v2-24-parser))
             => #t))
    (test-case "the closed ASCII lexer agrees with canonical tokens"
      (let ((seed 1729)
            (fast-count 0)
            (fallback-count 0)
            (alphabet "abcXYZ_eE0123= \t\r\n"))
        (def (next-random modulus)
          (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
          (modulo seed modulus))
        (for-each
         (lambda (source)
           (let (fast (direct-lex-hcl source))
             (if fast
               (begin
                 (set! fast-count (fx+ fast-count 1))
                 (unless (equal? fast (lex-source hcl-v2-24-parser source))
                   (error "generated lexer changed HCL tokens" source)))
               (set! fallback-count (fx+ fallback-count 1)))))
         '("key0 = 1\n" "a-b = 123\r\n" "a = 1.2\n"
           "a == 1\n" "a = \"text\"\n" "é = 1\n"))
        (let loop ((i 0))
          (when (< i 512)
            (let (source
                  (call-with-output-string
                   (lambda (port)
                     (let chars ((remaining (next-random 32)))
                       (when (> remaining 0)
                         (display
                          (string-ref alphabet
                                      (next-random (string-length alphabet)))
                          port)
                         (chars (fx- remaining 1)))))))
              (let (fast (direct-lex-hcl source))
                (if fast
                  (begin
                    (set! fast-count (fx+ fast-count 1))
                    (unless (equal? fast (lex-source hcl-v2-24-parser source))
                      (error "generated lexer changed HCL tokens" source)))
                  (set! fallback-count (fx+ fallback-count 1)))))
            (loop (fx+ i 1))))
        (check (> fast-count 64) => #t)
        (check (> fallback-count 0) => #t)))
    (test-case "the 1024-line Basic source retains its exact artifact"
      (let* ((source
              (call-with-output-string
               (lambda (port)
                 (let loop ((i 0))
                   (when (< i 1024)
                     (display "key" port)
                     (display i port)
                     (display " = 1\n" port)
                     (loop (fx+ i 1)))))))
             (artifact (parse-hcl-v2-24 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check artifact => (direct-parse-hcl hcl-v2-24-parser source))
        (check artifact => (parse-hcl-indexed-baseline source))))
    (test-case "compact HCL grows the rollback event vector losslessly"
      (let* ((source
              (call-with-output-string
               (lambda (port)
                 (let loop ((i 0))
                   (when (< i 128)
                     (display "key" port)
                     (display i port)
                     (display "=1\n" port)
                     (loop (fx+ i 1)))))))
             (artifact (parse-hcl-v2-24 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check artifact => (direct-parse-hcl hcl-v2-24-parser source))
        (check artifact => (parse-hcl-indexed-baseline source))))
    (test-case "generated events preserve Unicode byte offsets and trivia"
      (let* ((source "名称 = \"λ中😀\"\n# 注释\n")
             (artifact (parse-hcl-v2-24 source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check artifact => (parse-hcl-indexed-baseline source))))
    (test-case "the upstream syntax identity is immutable"
      (check +hcl-native-syntax-version+ => "v2.24.0")
      (check +hcl-native-syntax-commit+
             => "6b5068090eef06b1f127f61529db5ba0be7ed343")
      (check +hcl-syntax-contract+
             => "hcl-native-v2.24.0.v1")
      (check (language-parser-entry-ref hcl-v2-24-language 'schema)
             => +language-parser-entry-schema+)
      (check (language-parser-entry-ref hcl-v2-24-language 'language)
             => "hcl")
      (check (language-parser-entry-ref hcl-v2-24-language 'version)
             => +hcl-native-syntax-version+)
      (check (language-parser-entry-ref hcl-v2-24-language 'contract)
             => +hcl-syntax-contract+)
      (check (length hcl-v2-24-official-fixtures) => 15)
      (check (length hcl-v2-24-official-accepted-fixtures) => 12)
      (check (length hcl-v2-24-official-rejected-fixtures) => 3))
    (test-case "complete official HCL specsuite sources parse losslessly"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-hcl-v2-24 source))
                (accepted? (parse-artifact-success? artifact)))
           (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
           (check (syntax-fixture-contract fixture) => +hcl-syntax-contract+)
           (check (syntax-fixture-expected-status fixture) => 'accepted)
           (check (list (syntax-fixture-id fixture) accepted?)
                  => (list (syntax-fixture-id fixture) #t))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (check (parse-artifact-roundtrip artifact) => source)
           (check artifact => (direct-parse-hcl hcl-v2-24-parser source))
           (check artifact => (parse-hcl-indexed-baseline source))
           (when accepted?
             (let* ((root (parse-artifact->cst artifact))
                    (kinds (cst-node-kinds root)))
               (check (syntax-node-kind root)
                      => (syntax-fixture-root-kind fixture))
               (for-each
                (lambda (required-kind)
                  (check (member required-kind kinds) ? values))
                (syntax-fixture-required-kinds fixture))))))
       hcl-v2-24-official-accepted-fixtures))
    (test-case "official invalid HCL sources fail as one typed artifact"
      (for-each
       (lambda (fixture)
         (let* ((source (syntax-fixture-source fixture))
                (artifact (parse-hcl-v2-24 source)))
           (check (syntax-fixture-version fixture) => +hcl-native-syntax-version+)
           (check (syntax-fixture-contract fixture) => +hcl-syntax-contract+)
           (check (syntax-fixture-expected-status fixture) => 'rejected)
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-ref artifact 'sourceDigest)
                  => (syntax-fixture-source-digest fixture))
           (check (parse-artifact-roundtrip artifact) => source)
           (check (direct-parse-hcl hcl-v2-24-parser source) => #f)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       hcl-v2-24-official-rejected-fixtures))))
