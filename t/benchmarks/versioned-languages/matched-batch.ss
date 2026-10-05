#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete ParseArtifact workload for matched parser-revision comparisons.
;;; Run in separate processes with each revision's own GERBIL_PATH. Dynamic
;;; import and GC are outside the timed region; each sample parses all sources.

(import (only-in :gerbil-parser/language-support syntax-fixture-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip)
        (only-in :gerbil-parser/languages/hcl/parser
                 hcl-official-accepted-fixtures)
        (only-in :gerbil-parser/languages/hcl/parser parse-hcl)
        (only-in :gerbil-parser/languages/gql/parser
                 gql-official-fixtures)
        (only-in :gerbil-parser/languages/gql/parser
                 parse-gql)
        (only-in :gerbil-parser/languages/cypher/parser
                 opencypher-accepted-fixtures)
        (only-in :gerbil-parser/languages/cypher/parser
                 parse-opencypher)
        (only-in :gerbil-parser/languages/tla-plus/fixtures
                 tla-plus-core-accepted-fixtures)
        (only-in :gerbil-parser/languages/tla-plus/parser
                 parse-tla-plus-core))

(def corpus
  (list
   (cons parse-hcl
         (map syntax-fixture-source hcl-official-accepted-fixtures))
   (cons parse-gql
         (map syntax-fixture-source gql-official-fixtures))
   (cons parse-opencypher
         (map syntax-fixture-source opencypher-accepted-fixtures))
   (cons parse-tla-plus-core
         (map syntax-fixture-source tla-plus-core-accepted-fixtures))))

(def (run-batch)
  (for-each
   (lambda (group)
     (for-each
      (lambda (source)
        (let (artifact ((car group) source))
          (unless (and (parse-artifact-success? artifact)
                       (equal? (parse-artifact-roundtrip artifact) source))
            (error "matched ParseArtifact batch failed" source))))
      (cdr group)))
   corpus))

(def (main . args)
  (let ((label (if (null? args) "unnamed" (car args)))
        (samples (if (or (null? args) (null? (cdr args)))
                   20 (string->number (cadr args)))))
    (unless (and (integer? samples) (> samples 0))
      (error "expected positive sample count" args))
    (run-batch)
    (run-batch)
    (run-batch)
    (write
     (list (cons 'label label)
           (cons 'fixture-count
                 (fold (lambda (group total) (+ total (length (cdr group))))
                       0 corpus))
           (cons 'source-characters
                 (fold (lambda (group total)
                         (+ total
                            (fold (lambda (source n)
                                    (+ n (string-length source)))
                                  0 (cdr group))))
                       0 corpus))))
    (newline)
    (let loop ((sample 0))
      (when (< sample samples)
        (##gc)
        (let (started (##current-time-point))
          (run-batch)
          (write
           (list (cons 'sample sample)
                 (cons 'elapsed-ms
                       (* 1000.0 (- (##current-time-point) started)))))
          (newline))
        (loop (+ sample 1))))))

(export main)
