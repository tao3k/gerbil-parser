#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import (only-in :std/test
                 check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/gql/iso-39075-2024/parser
                 +gql-representative-query+
                 gql-iso-parser parse-gql-iso-39075-2024)
        (only-in :gerbil-parser/languages/hl7/v2-2.5.1/parser
                 hl7v2-parser parse-hl7v2)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser
                 hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-events parse-artifact-success? parse-artifact-valid?
                 parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/incremental
                 apply-edit make-edit parse-source/incremental
                 make-incremental-session incremental-session-artifact
                 parse-incremental-session)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/recovery parse-source/recover))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def recovery-incremental-test
  (test-suite "recovery and incremental v1 sidecars"
    (test-case "streamed fresh parse equals checkpointed source driver"
      (for-each
       (lambda (case)
         (let ((machine (car case)) (source (cdr case)))
           (check (parse-source machine source)
                  => (incremental-session-artifact
                      (make-incremental-session machine source)))))
       (list
        (cons arithmetic-parser "1 + 2 * (3 + 4)")
        (cons arithmetic-parser "1 +")
        (cons gql-iso-parser +gql-representative-query+)
        (cons hl7v2-parser
              "MSH|^~\\&|LEGACY|AU|FHIR|AU|202609170900||ADT^A08|1|P|2.5.1\r"))))
    (test-case "UTF-8 edits are byte-bound and reject split characters"
      (check (apply-edit "λx" (make-edit 0 2 "a")) => "ax")
      (check (apply-edit "aλ中😀z" (make-edit 3 7 "b")) => "aλbz")
      (check-exception (apply-edit "λx" (make-edit 0 1 "a")) true)
      (check-exception (apply-edit "aλ中😀z" (make-edit 4 0 "b")) true))
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
            (check (row-ref receipt 'freshFallback?) => #f)
            (check (row-ref receipt 'publicationSchema)
                   => "gerbil-parser.parse-artifact.v1")))))
    (test-case "equal-width middle edits converge and reuse the token suffix"
      (let* ((source
              (string-append
               "(" (string-join (make-list 100 "001") " + ") ")"))
             (base (parse-arithmetic-v1 source))
             (edit-start (+ 1 (* 50 6)))
             (source-edit (make-edit edit-start 3 "002")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (let (fresh
                (parse-arithmetic-v1
                 (apply-edit source source-edit)))
            (check artifact => fresh)
            (check (row-ref receipt 'suffixByteDelta) => 0)
            (check (> (row-ref receipt 'convergedSuffixTokenCount) 90) => #t)
            (check (row-ref receipt 'reusedSuffixTokenCount)
                   => (row-ref receipt 'convergedSuffixTokenCount))
            (check (row-ref receipt 'relocatedSuffixTokenCount) => 0)
            (check (< (row-ref receipt 'relexedByteCount) 128) => #t)
            (check (row-ref receipt 'resumedSignificantTokenCount) => 0)
            (check (> (row-ref receipt
                               'reusedRecognitionEventCount) 0) => #t)
            (check (< (row-ref receipt 'remainingSignificantTokenCount)
                      120)
                   => #t)))))
    (test-case "persistent checkpoints resume sequential middle edits"
      (let* ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 3 "0002 + 003")))
        (let-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (let* ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 120 6) 7) 3 "0003")))
            (check (incremental-session-artifact next)
                   => (parse-arithmetic-v1 first-source))
            (check (> (row-ref first-receipt
                               'checkpointReusedShiftCount) 0)
                   => #t)
            (let-values (((final second-receipt)
                          (parse-incremental-session next second-edit)))
              (check (incremental-session-artifact final)
                     => (parse-arithmetic-v1
                         (apply-edit first-source second-edit)))
              (check (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0)
                     => #t))))))
    (test-case "checkpoint byte cursors cover late tokens and trivia edits"
      (let* ((source (string-join (make-list 128 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (late-edit
              (make-edit (- (string-length source) 3) 3 "0002 + 003")))
        (let-values (((next receipt)
                      (parse-incremental-session session late-edit)))
          (let* ((late-source (apply-edit source late-edit))
                 (trivia-edit (make-edit 5 1 "  ")))
            (check (incremental-session-artifact next)
                   => (parse-arithmetic-v1 late-source))
            (check (> (row-ref receipt 'checkpointReusedShiftCount) 200)
                   => #t)
            (let-values (((final _receipt)
                          (parse-incremental-session next trivia-edit)))
              (check (incremental-session-artifact final)
                     => (parse-arithmetic-v1
                         (apply-edit late-source trivia-edit))))))))
    (test-case "same-width trivia edits reuse recognition across sessions"
      (let* ((source "001 + 002 + 003")
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit 3 1 "\t")))
        (let-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (let* ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit 9 1 "\t")))
            (check (incremental-session-artifact next)
                   => (parse-arithmetic-v1 first-source))
            (check (eq? (car (parse-artifact-events
                              (incremental-session-artifact session)))
                        (car (parse-artifact-events
                              (incremental-session-artifact next))))
                   => #t)
            (check (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0) => #t)
            (check (row-ref first-receipt 'relexedByteCount) => 1)
            (check (row-ref first-receipt 'relexStopByte) => 4)
            (check (row-ref first-receipt
                            'remainingSignificantTokenCount) => 0)
            (let-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (check (incremental-session-artifact again)
                     => (parse-arithmetic-v1
                         (apply-edit first-source second-edit)))
              (check (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0) => #t))))))
    (test-case "generic significant edits reuse events and retire later checkpoints"
      (let* ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 3 "002")))
        (let-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (let* ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (* 120 6) 3 "003")))
            (check (incremental-session-artifact next)
                   => (parse-arithmetic-v1 first-source))
            (check (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0) => #t)
            (check (row-ref first-receipt
                            'remainingSignificantTokenCount) => 1)
            (check (row-ref first-receipt 'relexedByteCount) => 3)
            (check (eq? (car (parse-artifact-events
                              (incremental-session-artifact session)))
                        (car (parse-artifact-events
                              (incremental-session-artifact next))))
                   => #t)
            (let-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (let* ((second-source
                      (apply-edit first-source second-edit))
                     (third-edit (make-edit (* 150 6) 3 "0004 + 005")))
                (check (incremental-session-artifact again)
                       => (parse-arithmetic-v1 second-source))
                (check (> (row-ref second-receipt
                                   'reusedRecognitionEventCount) 0) => #t)
                (let-values (((final third-receipt)
                              (parse-incremental-session again third-edit)))
                  (check (incremental-session-artifact final)
                         => (parse-arithmetic-v1
                             (apply-edit second-source third-edit)))
                  (check (> (row-ref third-receipt
                                     'checkpointReusedShiftCount) 0)
                         => #t))))))))
    (test-case "width-changing token edits relocate events without suffix LR"
      (for-each
       (lambda (case)
         (let* ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session arithmetic-parser source)))
           (let-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next)
                    => (parse-arithmetic-v1
                        (apply-edit source source-edit)))
             (check (parse-artifact-valid?
                     (incremental-session-artifact next)) => #t)
             (check (> (row-ref receipt
                                'reusedRecognitionEventCount) 0) => #t)
             (check (row-ref receipt 'resumedSignificantTokenCount) => 0)
             (check (row-ref receipt 'suffixByteDelta) => 1))))
       (list (list "001 + 002 + 003" (make-edit 6 3 "0002"))
             (list "001 + 002 + 003" (make-edit 3 1 "  "))
             (list "λ + 2 + 3" (make-edit 2 1 "  ")))))
    (test-case "certified multi-token window preserves LR event topology"
      (let* ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 9 "0002 + 0003")))
        (let-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (let* ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 120 6) 2) 9
                                         "004 + 005")))
            (check (incremental-session-artifact next)
                   => (parse-arithmetic-v1 first-source))
            (check (parse-artifact-valid?
                    (incremental-session-artifact next)) => #t)
            (check (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0) => #t)
            (check (row-ref first-receipt 'suffixByteDelta) => 2)
            (check (row-ref first-receipt
                            'remainingSignificantTokenCount) => 3)
            (let-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (check (incremental-session-artifact again)
                     => (parse-arithmetic-v1
                         (apply-edit first-source second-edit)))
              (check (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0)
                     => #t))))))
    (test-case "multi-token literal action change falls back to LR"
      (let* ((source "001 + 002 + 003")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 0 9 "004 * 005")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-arithmetic-v1
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'reusedRecognitionEventCount) => #f))))
    (test-case "contracted and Unicode token windows equal fresh artifacts"
      (for-each
       (lambda (case)
         (let* ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session arithmetic-parser source)))
           (let-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next)
                    => (parse-arithmetic-v1
                        (apply-edit source source-edit)))
             (check (parse-artifact-valid?
                     (incremental-session-artifact next)) => #t)
             (check (> (row-ref receipt
                                'reusedRecognitionEventCount) 0) => #t))))
       (list (list "0001 + 0002 + 0003"
                   (make-edit 0 11 "004 + 005"))
             (list "λ + 2 + 3"
                   (make-edit 0 6 "λ + 4")))))
    (test-case "trivia token count changes preserve HCL recognition"
      (let* ((source "x = 1 /*a*/\ny = 2\n")
             (session (make-incremental-session hcl-v2-24-parser source)))
        (for-each
         (lambda (source-edit)
           (let-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next)
                    => (parse-hcl-v2-24
                        (apply-edit source source-edit)))
             (check (parse-artifact-valid?
                     (incremental-session-artifact next)) => #t)
             (check (> (row-ref receipt
                                'reusedRecognitionEventCount) 0) => #t)
             (check (row-ref receipt
                             'remainingSignificantTokenCount) => 0)))
         (list (make-edit 6 5 "/*a*/ /*b*/")
               (make-edit 6 5 ""))))
      (let* ((source "x = 1 /*a*/ /*b*/\ny = 2\n")
             (session (make-incremental-session hcl-v2-24-parser source))
             (source-edit (make-edit 6 11 "/*c*/")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-hcl-v2-24 (apply-edit source source-edit)))
          (check (parse-artifact-valid?
                  (incremental-session-artifact next)) => #t)
          (check (> (row-ref receipt
                             'reusedRecognitionEventCount) 0) => #t))))
    (test-case "significant tokens align across changed trivia counts"
      (for-each
       (lambda (case)
         (let* ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session hcl-v2-24-parser source)))
           (let-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next)
                    => (parse-hcl-v2-24
                        (apply-edit source source-edit)))
             (check (parse-artifact-valid?
                     (incremental-session-artifact next)) => #t)
             (check (> (row-ref receipt
                                'reusedRecognitionEventCount) 0) => #t)
             (check (row-ref receipt
                             'remainingSignificantTokenCount) => 1))))
       (list
        (list "x = 1 /*a*/\ny = 2\n"
              (make-edit 4 7 "3 /*a*/ /*b*/"))
        (list "x = 3 /*a*/ /*b*/\ny = 2\n"
              (make-edit 4 13 "4 /*c*/")))))
    (test-case "changed LR literal action rejects mixed window reuse"
      (let* ((source "x = 1 /*a*/ + 2\n")
             (source-edit (make-edit 4 11 "3 /*a*/ /*b*/ *"))
             (session (make-incremental-session hcl-v2-24-parser source)))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-hcl-v2-24 (apply-edit source source-edit)))
          (check (row-ref receipt 'reusedRecognitionEventCount) => #f))))
    (test-case "literal action edits require LR re-execution"
      (let* ((source "001 + 002")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 4 1 "*")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-arithmetic-v1
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'reusedRecognitionEventCount) => #f))))
    (test-case "trivia fast path rejects a lexical kind change"
      (let* ((source "001 + 002")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 3 1 "+")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-arithmetic-v1
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'reusedRecognitionEventCount) => #f))))
    (test-case "Unicode and contextual trivia retain fresh parse products"
      (for-each
       (lambda (case)
         (let* ((machine (car case))
                (source (cadr case))
                (source-edit (caddr case))
                (session (make-incremental-session machine source)))
           (let-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next)
                    => (parse-source machine
                                     (apply-edit source source-edit)))
             (check (> (row-ref receipt
                                'reusedRecognitionEventCount) 0) => #t))))
       (list (list arithmetic-parser "λ + 2" (make-edit 2 1 "\t"))
             (list gql-iso-parser "CREATE GRAPH mygraph ANY"
                   (make-edit 6 1 "\t")))))
    (test-case "session falls back on a rejected edit"
      (let* ((source (string-join (make-list 96 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit (* 48 6) 3 "???")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-arithmetic-v1
                     (apply-edit source source-edit)))
          (check (parse-artifact-success?
                  (incremental-session-artifact next))
                 => #f)
          (check (row-ref receipt 'checkpointReusedShiftCount) => 0))))
    (test-case "rejected session can accept a repairing edit"
      (let* ((source "001 + ???")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 6 3 "002")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-arithmetic-v1 "001 + 002"))
          (check (row-ref receipt 'checkpointReusedShiftCount) => 0))))
    (test-case "session checkpoints survive inserted tokens and byte offsets"
      (let* ((source (string-join (make-list 96 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 48 6) 3 "001 + 002")))
        (let-values (((next _receipt)
                      (parse-incremental-session session first-edit)))
          (let* ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 72 6) 6) 3 "0003")))
            (let-values (((final receipt)
                          (parse-incremental-session next second-edit)))
              (check (incremental-session-artifact final)
                     => (parse-arithmetic-v1
                         (apply-edit first-source second-edit)))
              (check (> (row-ref receipt
                                 'reusedRecognitionEventCount) 0)
                     => #t))))))
    (test-case "GQL session preserves contextual parse after an identifier edit"
      (let* ((source "CREATE GRAPH mygraph ANY")
             (session (make-incremental-session gql-iso-parser source))
             (source-edit (make-edit 13 7 "newgraph")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       gql-iso-parser source
                       (parse-gql-iso-39075-2024 source) source-edit)))
          (check artifact
                 => (parse-gql-iso-39075-2024
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'freshFallback?) => #f))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-gql-iso-39075-2024
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'freshFallback?) => #f)
          (check (row-ref receipt 'checkpointReusedShiftCount) => 0))))
    (test-case "GQL edit after a GLR fork retains the ordinary path"
      (let* ((source "CREATE GRAPH mygraph ANY")
             (session (make-incremental-session gql-iso-parser source))
             (source-edit (make-edit 13 7 "yrgraph")))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-gql-iso-39075-2024
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'reusedRecognitionEventCount) => #f))))
    (test-case "GQL retains deterministic LR checkpoints before a GLR fork"
      (let* ((source +gql-representative-query+)
             (source-edit
              (make-edit (- (string-length source) 7) 6 "res"))
             (session (make-incremental-session gql-iso-parser source)))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (check (incremental-session-artifact next)
                 => (parse-gql-iso-39075-2024
                     (apply-edit source source-edit)))
          (check (> (row-ref receipt 'reusedRecognitionEventCount) 0) => #t)
          (check (row-ref receipt 'freshFallback?) => #f)
          (let (next-edit (make-edit 0 5 "match"))
            (let-values (((again _receipt)
                          (parse-incremental-session next next-edit)))
              (check (incremental-session-artifact again)
                     => (parse-gql-iso-39075-2024
                         (apply-edit
                          (apply-edit source source-edit)
                          next-edit))))))))
    (test-case "GQL contextual edits agree with fresh parsing across positions"
      (for-each
       (lambda (case)
         (let* ((source (car case))
                (source-edit (cdr case))
                (edited (apply-edit source source-edit))
                (fresh (parse-gql-iso-39075-2024 edited))
                (session (make-incremental-session gql-iso-parser source)))
           (let-values (((artifact _receipt)
                         (parse-source/incremental
                          gql-iso-parser source
                          (parse-gql-iso-39075-2024 source) source-edit)))
             (check artifact => fresh))
           (let-values (((next _receipt)
                         (parse-incremental-session session source-edit)))
             (check (incremental-session-artifact next) => fresh))))
       (list
        (cons "CREATE GRAPH mygraph ANY" (make-edit 0 6 "create"))
        (cons "CREATE GRAPH mygraph ANY" (make-edit 7 5 "GRAPH"))
        (cons "CREATE GRAPH mygraph ANY" (make-edit 13 7 "return"))
        (cons "match (n) return n\n" (make-edit 7 1 "CREATE"))
        (cons "match (n) return n\n" (make-edit 10 6 "RETURN")))))
    (test-case "UTF-8 byte shifts relocate rather than falsely reuse a suffix"
      (let* ((source "λ + 2")
             (base (parse-arithmetic-v1 source))
             (source-edit (make-edit 0 2 "name")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (check artifact
                 => (parse-arithmetic-v1
                     (apply-edit source source-edit)))
          (check (row-ref receipt 'suffixByteDelta) => 2)
          (check (> (row-ref receipt 'convergedSuffixTokenCount) 0) => #t)
          (check (row-ref receipt 'reusedSuffixTokenCount) => 0)
          (check (row-ref receipt 'relocatedSuffixTokenCount)
                 => (row-ref receipt 'convergedSuffixTokenCount)))))
    (test-case "token merge scans through the changed boundary"
      (let* ((source "name + 2 + 3")
             (base (parse-arithmetic-v1 source))
             (source-edit (make-edit 4 0 "x")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (check artifact => (parse-arithmetic-v1 "namex + 2 + 3"))
          (check (> (row-ref receipt 'relexedByteCount) 4) => #t)
          (check (< (row-ref receipt 'relexedByteCount)
                    (string-length (apply-edit source source-edit)))
                 => #t))))
    (test-case "external lexical scanners retain complete suffix re-lexing"
      (let* ((source
              "MSH|^~\\&|LEGACY|AU|FHIR|AU|202609170900||ADT^A08|1|P|2.5.1\r")
             (base (parse-hl7v2 source))
             (source-edit (make-edit 11 6 "NEWAPP")))
        (let-values (((artifact receipt)
                      (parse-source/incremental
                       hl7v2-parser source base source-edit)))
          (check artifact
                 => (parse-hl7v2 (apply-edit source source-edit)))
          (check (row-ref receipt 'relexStopByte)
                 => (string-length (apply-edit source source-edit))))))
    (test-case "missing literal recovery stays a rejected v1 publication"
      (let-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "(1")))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (row-ref receipt 'outcome) => 'candidate)
        (check (row-ref receipt 'publicationStatus) => 'rejected)
        (check (integer? (row-ref receipt 'frontierState)) => #t)
        (check (row-ref receipt 'reusedPrefixTokenCount) => 2)
        (check (<= (row-ref receipt 'attempts)
                   (length (row-ref receipt 'frontierExpectedTerminals)))
               => #t)
        (check (row-ref (car (row-ref receipt 'operations)) 'kind)
               => 'MISSING)))
    (test-case "skipped-token recovery records source-owned byte evidence"
      (let-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "1 ?")))
        (check (parse-artifact-success? artifact) => #f)
        (check (row-ref receipt 'outcome) => 'candidate)
        (check (row-ref receipt 'reusedPrefixTokenCount) => 1)
        (check (<= (row-ref receipt 'attempts)
                   (+ 1 (length
                         (row-ref receipt 'frontierExpectedTerminals))))
               => #t)
        (check (row-ref (car (row-ref receipt 'operations)) 'kind)
               => 'SKIPPED)))))

(export recovery-incremental-test)
