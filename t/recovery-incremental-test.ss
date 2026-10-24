#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import (only-in :std/test
                 check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-parser parse-arithmetic)
        (only-in :gerbil-parser/languages/gql/parser
                 +gql-representative-query+
                 gql-parser parse-gql)
        (only-in :gerbil-parser/languages/hl7/parser
                 hl7-parser parse-hl7)
        (only-in :gerbil-parser/languages/hcl/parser
                 hcl-parser parse-hcl)
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

(deflanguage-parser-tests recovery-incremental-test "recovery and incremental v1 sidecars"
  (engine)
    (scenario "streamed fresh parse equals checkpointed source driver"
      (each (case) ((list
        (cons arithmetic-parser "1 + 2 * (3 + 4)")
        (cons arithmetic-parser "1 +")
        (cons gql-parser +gql-representative-query+)
        (cons hl7-parser
              "MSH|^~\\&|LEGACY|AU|FHIR|AU|202609170900||ADT^A08|1|P|2.5.1\r")))
         (scope ((machine (car case)) (source (cdr case)))
           (equal (parse-source machine source)
                   (incremental-session-artifact
                      (make-incremental-session machine source))))))
    (scenario "UTF-8 edits are byte-bound and reject split characters"
      (equal (apply-edit "λx" (make-edit 0 2 "a"))  "ax")
      (equal (apply-edit "aλ中😀z" (make-edit 3 7 "b"))  "aλbz")
      (raises (apply-edit "λx" (make-edit 0 1 "a")) true)
      (raises (apply-edit "aλ中😀z" (make-edit 4 0 "b")) true))
    (scenario "incremental prefix reuse publishes fresh-equivalent v1"
      (sequential ((source "1 + 2")
             (base (parse-arithmetic source))
             (source-edit (make-edit 4 1 "30")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (scope ((fresh (parse-arithmetic "1 + 30")))
            (equal artifact  fresh)
            (equal (parse-artifact-valid? artifact)  #t)
            (equal (parse-artifact-roundtrip artifact)  "1 + 30")
            (equal (row-ref receipt 'schema)
                    "gerbil-parser.incremental-receipt.v1")
            (equal (row-ref receipt 'freshFallback?)  #f)
            (equal (row-ref receipt 'publicationSchema)
                    "gerbil-parser.parse-artifact.v1")))))
    (scenario "equal-width middle edits converge and reuse the token suffix"
      (sequential ((source
              (string-append
               "(" (string-join (make-list 100 "001") " + ") ")"))
             (base (parse-arithmetic source))
             (edit-start (+ 1 (* 50 6)))
             (source-edit (make-edit edit-start 3 "002")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (scope ((fresh
                (parse-arithmetic
                 (apply-edit source source-edit))))
            (equal artifact  fresh)
            (equal (row-ref receipt 'suffixByteDelta)  0)
            (equal (> (row-ref receipt 'convergedSuffixTokenCount) 90)  #t)
            (equal (row-ref receipt 'reusedSuffixTokenCount)
                    (row-ref receipt 'convergedSuffixTokenCount))
            (equal (row-ref receipt 'relocatedSuffixTokenCount)  0)
            (equal (< (row-ref receipt 'relexedByteCount) 128)  #t)
            (equal (row-ref receipt 'resumedSignificantTokenCount)  0)
            (equal (> (row-ref receipt
                               'reusedRecognitionEventCount) 0)  #t)
            (equal (< (row-ref receipt 'remainingSignificantTokenCount)
                      120)
                    #t)))))
    (scenario "persistent checkpoints resume sequential middle edits"
      (sequential ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 3 "0002 + 003")))
        (scope-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (sequential ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 120 6) 7) 3 "0003")))
            (equal (incremental-session-artifact next)
                    (parse-arithmetic first-source))
            (equal (> (row-ref first-receipt
                               'checkpointReusedShiftCount) 0)
                    #t)
            (scope-values (((final second-receipt)
                          (parse-incremental-session next second-edit)))
              (equal (incremental-session-artifact final)
                      (parse-arithmetic
                         (apply-edit first-source second-edit)))
              (equal (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0)
                      #t))))))
    (scenario "checkpoint byte cursors cover late tokens and trivia edits"
      (sequential ((source (string-join (make-list 128 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (late-edit
              (make-edit (- (string-length source) 3) 3 "0002 + 003")))
        (scope-values (((next receipt)
                      (parse-incremental-session session late-edit)))
          (sequential ((late-source (apply-edit source late-edit))
                 (trivia-edit (make-edit 5 1 "  ")))
            (equal (incremental-session-artifact next)
                    (parse-arithmetic late-source))
            (equal (> (row-ref receipt 'checkpointReusedShiftCount) 200)
                    #t)
            (scope-values (((final _receipt)
                          (parse-incremental-session next trivia-edit)))
              (equal (incremental-session-artifact final)
                      (parse-arithmetic
                         (apply-edit late-source trivia-edit))))))))
    (scenario "same-width trivia edits reuse recognition across sessions"
      (sequential ((source "001 + 002 + 003")
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit 3 1 "\t")))
        (scope-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (sequential ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit 9 1 "\t")))
            (equal (incremental-session-artifact next)
                    (parse-arithmetic first-source))
            (equal (eq? (car (parse-artifact-events
                              (incremental-session-artifact session)))
                        (car (parse-artifact-events
                              (incremental-session-artifact next))))
                    #t)
            (equal (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0)  #t)
            (equal (row-ref first-receipt 'relexedByteCount)  1)
            (equal (row-ref first-receipt 'relexStopByte)  4)
            (equal (row-ref first-receipt
                            'remainingSignificantTokenCount)  0)
            (scope-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (equal (incremental-session-artifact again)
                      (parse-arithmetic
                         (apply-edit first-source second-edit)))
              (equal (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0)  #t))))))
    (scenario "generic significant edits reuse events and retire later checkpoints"
      (sequential ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 3 "002")))
        (scope-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (sequential ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (* 120 6) 3 "003")))
            (equal (incremental-session-artifact next)
                    (parse-arithmetic first-source))
            (equal (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0)  #t)
            (equal (row-ref first-receipt
                            'remainingSignificantTokenCount)  1)
            (equal (row-ref first-receipt 'relexedByteCount)  3)
            (equal (eq? (car (parse-artifact-events
                              (incremental-session-artifact session)))
                        (car (parse-artifact-events
                              (incremental-session-artifact next))))
                    #t)
            (scope-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (sequential ((second-source
                      (apply-edit first-source second-edit))
                     (third-edit (make-edit (* 150 6) 3 "0004 + 005")))
                (equal (incremental-session-artifact again)
                        (parse-arithmetic second-source))
                (equal (> (row-ref second-receipt
                                   'reusedRecognitionEventCount) 0)  #t)
                (scope-values (((final third-receipt)
                              (parse-incremental-session again third-edit)))
                  (equal (incremental-session-artifact final)
                          (parse-arithmetic
                             (apply-edit second-source third-edit)))
                  (equal (> (row-ref third-receipt
                                     'checkpointReusedShiftCount) 0)
                          #t))))))))
    (scenario "width-changing token edits relocate events without suffix LR"
      (each (case) ((list (list "001 + 002 + 003" (make-edit 6 3 "0002"))
             (list "001 + 002 + 003" (make-edit 3 1 "  "))
             (list "λ + 2 + 3" (make-edit 2 1 "  "))))
         (sequential ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session arithmetic-parser source)))
           (scope-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)
                     (parse-arithmetic
                        (apply-edit source source-edit)))
             (equal (parse-artifact-valid?
                     (incremental-session-artifact next))  #t)
             (equal (> (row-ref receipt
                                'reusedRecognitionEventCount) 0)  #t)
             (equal (row-ref receipt 'resumedSignificantTokenCount)  0)
             (equal (row-ref receipt 'suffixByteDelta)  1)))))
    (scenario "certified multi-token window preserves LR event topology"
      (sequential ((source (string-join (make-list 160 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 80 6) 9 "0002 + 0003")))
        (scope-values (((next first-receipt)
                      (parse-incremental-session session first-edit)))
          (sequential ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 120 6) 2) 9
                                         "004 + 005")))
            (equal (incremental-session-artifact next)
                    (parse-arithmetic first-source))
            (equal (parse-artifact-valid?
                    (incremental-session-artifact next))  #t)
            (equal (> (row-ref first-receipt
                               'reusedRecognitionEventCount) 0)  #t)
            (equal (row-ref first-receipt 'suffixByteDelta)  2)
            (equal (row-ref first-receipt
                            'remainingSignificantTokenCount)  3)
            (scope-values (((again second-receipt)
                          (parse-incremental-session next second-edit)))
              (equal (incremental-session-artifact again)
                      (parse-arithmetic
                         (apply-edit first-source second-edit)))
              (equal (> (row-ref second-receipt
                                 'reusedRecognitionEventCount) 0)
                      #t))))))
    (scenario "multi-token literal action change falls back to LR"
      (sequential ((source "001 + 002 + 003")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 0 9 "004 * 005")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-arithmetic
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'reusedRecognitionEventCount)  #f))))
    (scenario "contracted and Unicode token windows equal fresh artifacts"
      (each (case) ((list (list "0001 + 0002 + 0003"
                   (make-edit 0 11 "004 + 005"))
             (list "λ + 2 + 3"
                   (make-edit 0 6 "λ + 4"))))
         (sequential ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session arithmetic-parser source)))
           (scope-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)
                     (parse-arithmetic
                        (apply-edit source source-edit)))
             (equal (parse-artifact-valid?
                     (incremental-session-artifact next))  #t)
             (equal (> (row-ref receipt
                                'reusedRecognitionEventCount) 0)  #t)))))
    (scenario "trivia token count changes preserve HCL recognition"
      (sequential ((source "x = 1 /*a*/\ny = 2\n")
             (session (make-incremental-session hcl-parser source)))
        (each (source-edit) ((list (make-edit 6 5 "/*a*/ /*b*/")
               (make-edit 6 5 "")))
           (scope-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)
                     (parse-hcl
                        (apply-edit source source-edit)))
             (equal (parse-artifact-valid?
                     (incremental-session-artifact next))  #t)
             (equal (> (row-ref receipt
                                'reusedRecognitionEventCount) 0)  #t)
             (equal (row-ref receipt
                             'remainingSignificantTokenCount)  0))))
      (sequential ((source "x = 1 /*a*/ /*b*/\ny = 2\n")
             (session (make-incremental-session hcl-parser source))
             (source-edit (make-edit 6 11 "/*c*/")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-hcl (apply-edit source source-edit)))
          (equal (parse-artifact-valid?
                  (incremental-session-artifact next))  #t)
          (equal (> (row-ref receipt
                             'reusedRecognitionEventCount) 0)  #t))))
    (scenario "significant tokens align across changed trivia counts"
      (each (case) ((list
        (list "x = 1 /*a*/\ny = 2\n"
              (make-edit 4 7 "3 /*a*/ /*b*/"))
        (list "x = 3 /*a*/ /*b*/\ny = 2\n"
              (make-edit 4 13 "4 /*c*/"))))
         (sequential ((source (car case))
                (source-edit (cadr case))
                (session (make-incremental-session hcl-parser source)))
           (scope-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)
                     (parse-hcl
                        (apply-edit source source-edit)))
             (equal (parse-artifact-valid?
                     (incremental-session-artifact next))  #t)
             (equal (> (row-ref receipt
                                'reusedRecognitionEventCount) 0)  #t)
             (equal (row-ref receipt
                             'remainingSignificantTokenCount)  1)))))
    (scenario "changed LR literal action rejects mixed window reuse"
      (sequential ((source "x = 1 /*a*/ + 2\n")
             (source-edit (make-edit 4 11 "3 /*a*/ /*b*/ *"))
             (session (make-incremental-session hcl-parser source)))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-hcl (apply-edit source source-edit)))
          (equal (row-ref receipt 'reusedRecognitionEventCount)  #f))))
    (scenario "literal action edits require LR re-execution"
      (sequential ((source "001 + 002")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 4 1 "*")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-arithmetic
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'reusedRecognitionEventCount)  #f))))
    (scenario "trivia fast path rejects a lexical kind change"
      (sequential ((source "001 + 002")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 3 1 "+")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-arithmetic
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'reusedRecognitionEventCount)  #f))))
    (scenario "Unicode and contextual trivia retain fresh parse products"
      (each (case) ((list (list arithmetic-parser "λ + 2" (make-edit 2 1 "\t"))
             (list gql-parser "CREATE GRAPH mygraph ANY"
                   (make-edit 6 1 "\t"))))
         (sequential ((machine (car case))
                (source (cadr case))
                (source-edit (caddr case))
                (session (make-incremental-session machine source)))
           (scope-values (((next receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)
                     (parse-source machine
                                     (apply-edit source source-edit)))
             (equal (> (row-ref receipt
                                'reusedRecognitionEventCount) 0)  #t)))))
    (scenario "session falls back on a rejected edit"
      (sequential ((source (string-join (make-list 96 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit (* 48 6) 3 "???")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-arithmetic
                     (apply-edit source source-edit)))
          (equal (parse-artifact-success?
                  (incremental-session-artifact next))
                  #f)
          (equal (row-ref receipt 'checkpointReusedShiftCount)  0))))
    (scenario "rejected session can accept a repairing edit"
      (sequential ((source "001 + ???")
             (session (make-incremental-session arithmetic-parser source))
             (source-edit (make-edit 6 3 "002")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-arithmetic "001 + 002"))
          (equal (row-ref receipt 'checkpointReusedShiftCount)  0))))
    (scenario "session checkpoints survive inserted tokens and byte offsets"
      (sequential ((source (string-join (make-list 96 "001") " + "))
             (session (make-incremental-session arithmetic-parser source))
             (first-edit (make-edit (* 48 6) 3 "001 + 002")))
        (scope-values (((next _receipt)
                      (parse-incremental-session session first-edit)))
          (sequential ((first-source (apply-edit source first-edit))
                 (second-edit (make-edit (+ (* 72 6) 6) 3 "0003")))
            (scope-values (((final receipt)
                          (parse-incremental-session next second-edit)))
              (equal (incremental-session-artifact final)
                      (parse-arithmetic
                         (apply-edit first-source second-edit)))
              (equal (> (row-ref receipt
                                 'reusedRecognitionEventCount) 0)
                      #t))))))
    (scenario "GQL session preserves contextual parse after an identifier edit"
      (sequential ((source "CREATE GRAPH mygraph ANY")
             (session (make-incremental-session gql-parser source))
             (source-edit (make-edit 13 7 "newgraph")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       gql-parser source
                       (parse-gql source) source-edit)))
          (equal artifact
                  (parse-gql
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'freshFallback?)  #f))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-gql
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'freshFallback?)  #f)
          (equal (row-ref receipt 'checkpointReusedShiftCount)  0))))
    (scenario "GQL edit after a GLR fork retains the ordinary path"
      (sequential ((source "CREATE GRAPH mygraph ANY")
             (session (make-incremental-session gql-parser source))
             (source-edit (make-edit 13 7 "yrgraph")))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-gql
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'reusedRecognitionEventCount)  #f))))
    (scenario "GQL retains deterministic LR checkpoints before a GLR fork"
      (sequential ((source +gql-representative-query+)
             (source-edit
              (make-edit (- (string-length source) 7) 6 "res"))
             (session (make-incremental-session gql-parser source)))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit)))
          (equal (incremental-session-artifact next)
                  (parse-gql
                     (apply-edit source source-edit)))
          (equal (> (row-ref receipt 'reusedRecognitionEventCount) 0)  #t)
          (equal (row-ref receipt 'freshFallback?)  #f)
          (scope ((next-edit (make-edit 0 5 "match")))
            (scope-values (((again _receipt)
                          (parse-incremental-session next next-edit)))
              (equal (incremental-session-artifact again)
                      (parse-gql
                         (apply-edit
                          (apply-edit source source-edit)
                          next-edit))))))))
    (scenario "GQL contextual edits agree with fresh parsing across positions"
      (each (case) ((list
        (cons "CREATE GRAPH mygraph ANY" (make-edit 0 6 "create"))
        (cons "CREATE GRAPH mygraph ANY" (make-edit 7 5 "GRAPH"))
        (cons "CREATE GRAPH mygraph ANY" (make-edit 13 7 "return"))
        (cons "match (n) return n\n" (make-edit 7 1 "CREATE"))
        (cons "match (n) return n\n" (make-edit 10 6 "RETURN"))))
         (sequential ((source (car case))
                (source-edit (cdr case))
                (edited (apply-edit source source-edit))
                (fresh (parse-gql edited))
                (session (make-incremental-session gql-parser source)))
           (scope-values (((artifact _receipt)
                         (parse-source/incremental
                          gql-parser source
                          (parse-gql source) source-edit)))
             (equal artifact  fresh))
           (scope-values (((next _receipt)
                         (parse-incremental-session session source-edit)))
             (equal (incremental-session-artifact next)  fresh)))))
    (scenario "UTF-8 byte shifts relocate rather than falsely reuse a suffix"
      (sequential ((source "λ + 2")
             (base (parse-arithmetic source))
             (source-edit (make-edit 0 2 "name")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (equal artifact
                  (parse-arithmetic
                     (apply-edit source source-edit)))
          (equal (row-ref receipt 'suffixByteDelta)  2)
          (equal (> (row-ref receipt 'convergedSuffixTokenCount) 0)  #t)
          (equal (row-ref receipt 'reusedSuffixTokenCount)  0)
          (equal (row-ref receipt 'relocatedSuffixTokenCount)
                  (row-ref receipt 'convergedSuffixTokenCount)))))
    (scenario "token merge scans through the changed boundary"
      (sequential ((source "name + 2 + 3")
             (base (parse-arithmetic source))
             (source-edit (make-edit 4 0 "x")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       arithmetic-parser source base source-edit)))
          (equal artifact  (parse-arithmetic "namex + 2 + 3"))
          (equal (> (row-ref receipt 'relexedByteCount) 4)  #t)
          (equal (< (row-ref receipt 'relexedByteCount)
                    (string-length (apply-edit source source-edit)))
                  #t))))
    (scenario "external lexical scanners retain complete suffix re-lexing"
      (sequential ((source
              "MSH|^~\\&|LEGACY|AU|FHIR|AU|202609170900||ADT^A08|1|P|2.5.1\r")
             (base (parse-hl7 source))
             (source-edit (make-edit 11 6 "NEWAPP")))
        (scope-values (((artifact receipt)
                      (parse-source/incremental
                       hl7-parser source base source-edit)))
          (equal artifact
                  (parse-hl7 (apply-edit source source-edit)))
          (equal (row-ref receipt 'relexStopByte)
                  (string-length (apply-edit source source-edit))))))
    (scenario "missing literal recovery stays a rejected v1 publication"
      (scope-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "(1")))
        (equal (parse-artifact-success? artifact)  #f)
        (equal (parse-artifact-valid? artifact)  #t)
        (equal (row-ref receipt 'outcome)  'candidate)
        (equal (row-ref receipt 'publicationStatus)  'rejected)
        (equal (integer? (row-ref receipt 'frontierState))  #t)
        (equal (row-ref receipt 'reusedPrefixTokenCount)  2)
        (equal (<= (row-ref receipt 'attempts)
                   (length (row-ref receipt 'frontierExpectedTerminals)))
                #t)
        (equal (row-ref (car (row-ref receipt 'operations)) 'kind)
                'MISSING)))
    (scenario "skipped-token recovery records source-owned byte evidence"
      (scope-values (((artifact receipt)
                    (parse-source/recover arithmetic-parser "1 ?")))
        (equal (parse-artifact-success? artifact)  #f)
        (equal (row-ref receipt 'outcome)  'candidate)
        (equal (row-ref receipt 'reusedPrefixTokenCount)  1)
        (equal (<= (row-ref receipt 'attempts)
                   (+ 1 (length
                         (row-ref receipt 'frontierExpectedTerminals))))
                #t)
        (equal (row-ref (car (row-ref receipt 'operations)) 'kind)
                'SKIPPED))))
