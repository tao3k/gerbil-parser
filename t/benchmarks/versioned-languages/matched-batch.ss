#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete ParseArtifact workload for matched parser-revision comparisons.
;;; Run in separate processes with each revision's own GERBIL_PATH. Dynamic
;;; import and GC are outside the timed region; each sample parses all sources.

(import (only-in :gerbil-parser/language-support/development
                 language-loader-fixtures language-loader-fixture-count)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-source)
        (only-in :gerbil-parser/languages/gql/parser-test gql-test-language)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip)
        (only-in :gerbil-parser/languages/hcl/parser
                 hcl-language parse-hcl)
        (only-in :gerbil-parser/languages/gql/parser
                 gql-language parse-gql)
        (only-in :gerbil-parser/languages/cypher/parser
                 opencypher-language parse-opencypher)
        (only-in :gerbil-parser/languages/tla-plus/parser
                 tla-plus-core-language parse-tla-plus-core))

(def corpus
  (list
   (cons parse-hcl
         (map syntax-fixture-source (language-loader-fixtures hcl-language 'accepted)))
   (cons parse-gql
         (map syntax-fixture-source (language-loader-fixtures gql-test-language 'accepted)))
   (cons parse-opencypher
         (map syntax-fixture-source (language-loader-fixtures opencypher-language 'accepted)))
   (cons parse-tla-plus-core
         (map syntax-fixture-source (language-loader-fixtures tla-plus-core-language 'accepted)))))

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
