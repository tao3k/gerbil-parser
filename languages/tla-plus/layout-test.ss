#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; The layout contract groups conjunction and disjunction items by SANY's
;;; marker end column while preserving the exact source bytes.

(import (only-in :std/test check test-case test-suite)
        (only-in :std/sync/barrier
                 barrier-post! barrier-wait! make-barrier)
        :gerbil-parser/languages/tla-plus/parser
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/incremental
                 make-edit parse-source/incremental)
        (only-in :gerbil-parser/src/runtime/recovery
                 parse-source/recover))
(export tla-plus-layout-parser-test)

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

(def (check-layout-shape source expected)
  (let (artifact (parse-tla-plus-layout source))
    (check (parse-artifact-success? artifact) => #t)
    (check (parse-artifact-valid? artifact) => #t)
    (check (parse-artifact-roundtrip artifact) => source)
    (check (map junction-body-count
                (junctions (parse-artifact->cst artifact)))
           => expected)))

(def tla-plus-layout-parser-test
  (test-suite "TLA+ column layout grammar"
    (test-case "aligned markers make one list"
      (check-layout-shape
       "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
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
    (test-case "concurrent requests keep layout columns and frames isolated"
      (let* ((sources
              (list
               "---- MODULE A ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
               "---- MODULE B ----\nInit ==\n  \\/ TRUE\n  \\/ FALSE\n====\n"
               "---- MODULE C ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"))
             (expected (map parse-tla-plus-layout sources))
             (barrier (make-barrier 9))
             (workers
              (map
               (lambda (index)
                 (spawn
                  (lambda ()
                    (barrier-post! barrier)
                    (barrier-wait! barrier)
                    (parse-tla-plus-layout
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
             (base (parse-tla-plus-layout source))
             (offset (string-contains source "FALSE"))
             (edited
              (string-append (substring source 0 offset) "TRUE"
                             (substring source (+ offset 5)
                                        (string-length source)))))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       tla-plus-layout-parser source base
                       (make-edit offset 5 "TRUE"))))
          (check artifact => (parse-tla-plus-layout edited))
          (check (parse-artifact-valid? artifact) => #t)
          (check (parse-artifact-roundtrip artifact) => edited)
          (check (cdr (assq 'freshFallback? receipt)) => #t))))
    (test-case "dangling list item remains rejected"
      (let (artifact
            (parse-tla-plus-layout
             "---- MODULE J ----\nInit ==\n  /\\\n====\n"))
        (check (parse-artifact-success? artifact) => #f)))
    (test-case "recovery keeps layout rejection unchanged"
      (let-values (((artifact receipt)
                    (parse-source/recover
                     tla-plus-layout-parser
                     "---- MODULE J ----\nInit ==\n  /\\\n====\n")))
        (check (parse-artifact-success? artifact) => #f)
        (check (cdr (assq 'outcome receipt)) => 'disabled)))))
