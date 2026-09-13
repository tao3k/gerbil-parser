#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import (only-in :std/test
                 check check-exception run-tests! test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid?
                 parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/incremental
                 apply-edit make-edit parse-source/incremental)
        (only-in :gerbil-parser/src/runtime/recovery parse-source/recover))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def recovery-incremental-tests
  (test-suite "recovery and incremental v1 sidecars"
    (test-case "UTF-8 edits are byte-bound and reject split characters"
      (check (apply-edit "λx" (make-edit 0 2 "a")) => "ax")
      (check-exception (apply-edit "λx" (make-edit 0 1 "a")) true))
    (test-case "incremental prefix reuse publishes fresh-equivalent v1"
      (let* ((source "1 + 2")
             (base (parse-arithmetic-v1 source))
             (source-edit (make-edit 4 1 "30")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (let (fresh (parse-arithmetic-v1 "1 + 30"))
            (check artifact => fresh)
            (check (parse-artifact-valid? artifact) => #t)
            (check (parse-artifact-roundtrip artifact) => "1 + 30")
            (check (row-ref receipt 'schema)
                   => "gerbil-parser.incremental-receipt.v1")
            (check (> (row-ref receipt 'reusedTokenCount) 0) => #t)
            (check (row-ref receipt 'publicationSchema)
                   => "gerbil-parser.parse-artifact.v1")))))
    (test-case "missing literal recovery stays a rejected v1 publication"
      (let-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "(1")))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (row-ref receipt 'outcome) => 'candidate)
        (check (row-ref receipt 'publicationStatus) => 'rejected)
        (check (row-ref (car (row-ref receipt 'operations)) 'kind)
               => 'MISSING)))
    (test-case "skipped-token recovery records source-owned byte evidence"
      (let-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "1 ?")))
        (check (parse-artifact-success? artifact) => #f)
        (check (row-ref receipt 'outcome) => 'candidate)
        (check (row-ref (car (row-ref receipt 'operations)) 'kind)
               => 'SKIPPED)))))

(run-tests! recovery-incremental-tests)
