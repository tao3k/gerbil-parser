#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; The layout contract groups conjunction and disjunction items by SANY's
;;; marker end column while preserving the exact source bytes.

(import (only-in :std/test check test-case test-suite)
        (only-in :std/sync/barrier
                 barrier-post! barrier-wait! make-barrier)
        :gerbil-parser/languages/tla-plus/sany-candidate
        (only-in :gerbil-parser/languages/tla-plus/grammars/layout
                 tla-plus-layout-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-contract language-grammar-version)
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/incremental
                 make-edit parse-source/incremental)
        (only-in :gerbil-parser/src/runtime/recovery
                 parse-source/recover))
(export tla-plus-sany-candidate-parser-test)

(def (junctions value)
  (cond
   ((syntax-node? value)
    (append
     (if (eq? (syntax-node-kind value) 'JunctionExpression)
       (list value) '())
     (apply append (map junctions (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map junctions (syntax-field-children value))))
   (else '())))

(def (junction-body-count value)
  (length
   (filter (lambda (child)
             (and (syntax-field? child)
                  (eq? (syntax-field-name child) 'body)))
           (syntax-node-children value))))

(def (node-kind-count value kind)
  (cond
   ((syntax-node? value)
    (+ (if (eq? (syntax-node-kind value) kind) 1 0)
       (apply + (map (lambda (child) (node-kind-count child kind))
                     (syntax-node-children value)))))
   ((syntax-field? value)
    (apply + (map (lambda (child) (node-kind-count child kind))
                  (syntax-field-children value))))
   (else 0)))

(def (check-layout-shape source expected)
  (let (artifact (parse-tla-plus-sany-candidate source))
    (check (parse-artifact-success? artifact) => #t)
    (check (parse-artifact-valid? artifact) => #t)
    (check (parse-artifact-roundtrip artifact) => source)
    (check (map junction-body-count
                (junctions (parse-artifact->cst artifact)))
           => expected)))

(def tla-plus-sany-candidate-parser-test
  (test-suite "TLA+ SANY candidate grammar"
    (test-case "candidate identity is distinct from the published layout contract"
      (check (equal?
              (language-grammar-contract
               tla-plus-sany-candidate-language-grammar)
              (language-grammar-contract tla-plus-layout-language-grammar))
             => #f)
      (check (string?
              (language-grammar-version
               tla-plus-sany-candidate-language-grammar))
             => #t))
    (test-case "aligned markers make one list"
      (check-layout-shape
       "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
       '(2)))
    (test-case "an explicit action closer ends an aligned list on the same line"
      (check-layout-shape
       "---- MODULE J ----\nCONSTANT S\nVARIABLE x\nQ == [][ /\\ (\\A i \\in S : i = i)\n         /\\ (\\A j \\in S : j = j) ]_<<x>>\n====\n"
       '(2)))
    (test-case "nested quantifier lists close at the outer reference"
      (check-layout-shape
       "---- MODULE J ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"
       '(2 2)))
    (test-case "different marker columns do not join one list"
      (check-layout-shape
       "---- MODULE J ----\nInit ==\n  /\\ TRUE\n   /\\ FALSE\n====\n"
       '(1)))
    (test-case "CRLF and tab stops retain marker alignment"
      (check-layout-shape
       "---- MODULE J ----\r\nInit ==\r\n\t/\\ TRUE\r\n\t/\\ FALSE\r\n====\r\n"
       '(2)))
    (test-case "variable-width borders and function-set syntax"
      (let* ((source
              "-------- MODULE J --------\n----------------\nF == [S -> T]\n==========\n")
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'FunctionSetExpression)
               => 1)))
    (test-case "operator constants, EXCEPT updates, and field selection"
      (let* ((source
              (string-append
               "---- MODULE J ----\n"
               "CONSTANTS Send(_, _), T\n"
               "F == [pc EXCEPT ![self] = \"b1\"]\n"
               "G == participant[i].decision\n====\n"))
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'ExceptExpression)
               => 1)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'RecordFieldExpression)
               => 1)))
    (test-case "instantiated module operator qualification"
      (let* ((source
              "---- MODULE J ----\nSpec == Inner(q)!ISpec\n====\n")
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'InstanceQualifiedExpression)
               => 1)))
    (test-case "infix operator definitions and qualified symbolic operators"
      (let* ((source
              (string-append
               "---- MODULE N ----\n"
               "a \\div b == R!\\div(a, b)\n"
               "a % b == a - b * (a \\div b)\n"
               "a < b == (a \\leq b) /\\ (a # b)\n====\n"))
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'OperatorDefinition)
               => 3)))
    (test-case "record sets keep colon fields distinct from record values"
      (let* ((source
              (string-append
               "---- MODULE R ----\n"
               "Type == [val : Data, rdy : {0, 1}]\n"
               "Value == [val |-> x, rdy |-> TRUE]\n====\n"))
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'RecordSetExpression)
               => 1)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'RecordExpression)
               => 1)))
    (test-case "temporal leads-to is one lossless infix operator"
      (let* ((source
              (string-append
               "---- MODULE Leads ----\n"
               "Leads == A ~> B\n"
               "Grouped == (A /\\ B) ~> C\n====\n"))
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'Expression)
               => 3)))
    (test-case "action subscripts require SANY's closing delimiter"
      (check (parse-artifact-success?
              (parse-tla-plus-sany-candidate
               "---- MODULE Bad ----\nP == [A]x\n====\n"))
             => #f)
      (let* ((source
              "---- MODULE Tuple ----\nP == [A]_<<x,y>>\nQ == [][A]_(<<x,y>>)\nR == <><<A>>_x\n====\n")
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'TemporalSubscriptExpression)
               => 2)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'AngleActionExpression)
               => 1)))
    (test-case "function definitions retain domain bindings in LET"
      (let* ((source
              (string-append
               "---- MODULE Functions ----\n"
               "F[x \\in S, y \\in T] == <<x,y>>\n"
               "L == LET G[x \\in S] == x H[y \\in T] == y IN <<G,H>>\n====\n"))
             (artifact (parse-tla-plus-sany-candidate source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'FunctionDefinition)
               => 1)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'LocalFunctionDefinition)
               => 2)
        (check (node-kind-count (parse-artifact->cst artifact)
                                'DomainBinding)
               => 4)))
    (test-case "concurrent requests keep layout columns and frames isolated"
      (let* ((sources
              (list
               "---- MODULE A ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
               "---- MODULE B ----\nInit ==\n  \\/ TRUE\n  \\/ FALSE\n====\n"
               "---- MODULE C ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"))
             (expected (map parse-tla-plus-sany-candidate sources))
             (barrier (make-barrier 9))
             (workers
              (map
               (lambda (index)
                 (spawn
                  (lambda ()
                    (barrier-post! barrier)
                    (barrier-wait! barrier)
                    (parse-tla-plus-sany-candidate
                     (list-ref sources (modulo index 3))))))
               (iota 9)))
             (actual (map thread-join! workers)))
        (check (map parse-artifact-valid? actual)
               => (make-list 9 #t))
        (check actual
               => (map (lambda (index)
                         (list-ref expected (modulo index 3)))
                       (iota 9)))))
    (test-case "edits reparse with the same layout semantics"
      (let* ((source
              "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n")
             (base (parse-tla-plus-sany-candidate source))
             (offset (string-contains source "FALSE"))
             (edited
              (string-append (substring source 0 offset) "TRUE"
                             (substring source (+ offset 5)
                                        (string-length source)))))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       tla-plus-sany-candidate-parser source base
                       (make-edit offset 5 "TRUE"))))
          (check artifact => (parse-tla-plus-sany-candidate edited))
          (check (parse-artifact-valid? artifact) => #t)
          (check (parse-artifact-roundtrip artifact) => edited)
          (check (cdr (assq 'freshFallback? receipt)) => #t))))
    (test-case "dangling list item remains rejected"
      (let (artifact
            (parse-tla-plus-sany-candidate
             "---- MODULE J ----\nInit ==\n  /\\\n====\n"))
        (check (parse-artifact-success? artifact) => #f)))
    (test-case "recovery keeps layout rejection unchanged"
      (let-values (((artifact receipt)
                    (parse-source/recover
                     tla-plus-sany-candidate-parser
                     "---- MODULE J ----\nInit ==\n  /\\\n====\n")))
        (check (parse-artifact-success? artifact) => #f)
        (check (cdr (assq 'outcome receipt)) => 'disabled)))))
