#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Observational syntax audit over a commit-bound extracted TCK manifest.

(import (only-in :gerbil-parser/languages/cypher/opencypher-2024-1/parser
                 parse-opencypher-2024-1)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-ref
                 parse-artifact-roundtrip
                 parse-artifact-success?
                 parse-artifact-valid?
                 sha256-text))

(def expected-schema "gerbil-parser.opencypher-tck-query-manifest.v1")
(def expected-commit "30b451d3b7c94ee5a84a0fdc223947a442dd9493")

;; : (forall (a) (-> [(Pair Symbol a)] Symbol a))
(def (row-ref row key)
  (alet (entry (assq key row)) (cdr entry)))

;; : (-> String String)
(def (path-category path)
  (let loop ((offset 0) (slashes 0))
    (cond
     ((= offset (string-length path)) path)
     ((char=? (string-ref path offset) #\/)
      (if (= slashes 1)
        (substring path 0 offset)
        (loop (fx+ offset 1) (fx+ slashes 1))))
     (else (loop (fx+ offset 1) slashes)))))

;; : (-> String List List)
(def (increment key rows)
  (cond
   ((null? rows) (list (cons key 1)))
   ((equal? key (caar rows))
    (cons (cons key (fx+ 1 (cdar rows))) (cdr rows)))
   (else (cons (car rows) (increment key (cdr rows))))))

;; : (-> String Datum)
(def (read-manifest path)
  (call-with-input-file
   path
   (lambda (port)
     (let ((manifest (read port)) (trailing (read port)))
       (unless (eof-object? trailing)
         (error "TCK manifest contains trailing data" path))
       manifest))))

;; Record layout is fixed by opencypher-tck-extract.rb and intentionally
;; positional so the generated manifest stays compact.
(def record-identity car)
(def record-path cadr)
(def record-scenario-line caddr)
(def record-name cadddr)
(def (record-example-id row) (list-ref row 4))
(def (record-query-line row) (list-ref row 6))
(def (record-skip? row) (list-ref row 7))
(def (record-outline? row) (list-ref row 8))
(def (record-unresolved? row) (list-ref row 9))
(def (record-outcome row) (list-ref row 10))
(def (record-error-class row) (list-ref row 12))
(def (record-error-reason row) (list-ref row 13))
(def (record-source-digest row) (list-ref row 14))
(def (record-source row) (list-ref row 15))

;; : (-> String String Boolean Boolean)
(def (expected-syntax-acceptance? outcome skip? unresolved?)
  (and (not skip?)
       (not unresolved?)
       (member outcome '("expected-success" "runtime-error"))))

;; : (-> String Alist)
(def (audit path)
  (let* ((manifest (read-manifest path))
         (schema (row-ref manifest 'schema))
         (commit (row-ref manifest 'upstreamCommit))
         (records (cdr (assq 'records manifest)))
         (record-count (length records))
         (started (##current-time-point)))
    (unless (and (equal? schema expected-schema)
                 (equal? commit expected-commit)
                 (= (row-ref manifest 'recordCount) (length records)))
      (error "invalid openCypher TCK manifest identity" manifest))
    (let ((seen-sources (make-hash-table))
          (seen-unexpected-sources (make-hash-table)))
      (let loop ((rest records)
                 (index 0)
                 (unique-sources 0) (unique-unexpected-sources 0)
                 (accepted 0) (rejected 0)
                 (expected-accepted 0) (expected-rejected 0)
                 (compile-accepted 0) (compile-rejected 0)
                 (skipped 0) (unresolved 0)
                 (categories '()) (failure-kinds '()) (token-kinds '())
                 (token-lexemes '()) (states '()) (expected-terminals '())
                 (ambiguity-branch-sites '())
                 (failures '()))
      (if (null? rest)
        (list
         (cons 'schema "gerbil-parser.opencypher-tck-syntax-audit.v1")
         (cons 'upstreamCommit commit)
         (cons 'featureTreeDigest (row-ref manifest 'featureTreeDigest))
         (cons 'featureFileCount (row-ref manifest 'featureFileCount))
                 (cons 'expandedQueryCount (+ accepted rejected))
         (cons 'uniqueSourceCount unique-sources)
         (cons 'accepted accepted)
         (cons 'rejected rejected)
         (cons 'expectedSyntaxAcceptanceAccepted expected-accepted)
         (cons 'expectedSyntaxAcceptanceRejected expected-rejected)
         (cons 'compileErrorParserAccepted compile-accepted)
         (cons 'compileErrorParserRejected compile-rejected)
         (cons 'skipGrammarCheck skipped)
         (cons 'unresolvedPlaceholders unresolved)
         (cons 'unexpectedUniqueSourceCount unique-unexpected-sources)
         (cons 'unexpectedRejectionsByCategory categories)
         (cons 'unexpectedRejectionsByFailureKind failure-kinds)
         (cons 'unexpectedRejectionsByTokenKind token-kinds)
         (cons 'unexpectedRejectionsByTokenLexeme token-lexemes)
         (cons 'unexpectedRejectionsByState states)
         (cons 'unexpectedRejectionsByExpectedTerminals expected-terminals)
         (cons 'unexpectedRejectionsByAmbiguityBranchSite
               ambiguity-branch-sites)
         (cons 'firstUnexpectedRejections (reverse failures)))
        (let* ((record (car rest))
               (_progress
                (when (zero? (modulo index 100))
                  (write
                   (list 'opencypher-tck-progress
                         (cons 'processed index)
                         (cons 'total record-count)
                         (cons 'elapsedMilliseconds
                               (floor
                                (* 1000.0
                                   (- (##current-time-point) started))))
                         (cons 'nextRecord (record-identity record))))
                  (newline)
                  (force-output)))
               (source (record-source record))
               (outcome (record-outcome record))
               (skip? (record-skip? record))
               (unresolved? (record-unresolved? record))
               (expected? (expected-syntax-acceptance?
                           outcome skip? unresolved?))
               (artifact (parse-opencypher-2024-1 source))
               (ok? (parse-artifact-success? artifact))
               (diagnostic
                (and (not ok?)
                     (car (parse-artifact-ref artifact 'diagnostics))))
               (failure-kind
                (or (and diagnostic
                         (alet (entry (assq 'failureKind diagnostic))
                           (cdr entry)))
                    'unclassified))
               (token-kind
                (or (and diagnostic
                         (alet (entry (assq 'tokenKind diagnostic))
                           (cdr entry)))
                    'unavailable))
               (token-lexeme
                (or (and diagnostic
                         (alet (entry (assq 'tokenLexeme diagnostic))
                           (cdr entry)))
                    "<unavailable>"))
               (state
                (or (and diagnostic
                         (alet (entry (assq 'state diagnostic)) (cdr entry)))
                    -1))
               (expected
                (or (and diagnostic
                         (alet (entry (assq 'expectedTerminals diagnostic))
                           (cdr entry)))
                    '()))
               (diagnostic-branch-sites
                (or (and diagnostic
                         (alet (entry (assq 'branchSites diagnostic))
                           (cdr entry)))
                    '()))
               (digest (record-source-digest record))
               (new-source? (not (hash-key? seen-sources digest))))
          (unless (equal? (sha256-text source) (record-source-digest record))
            (error "TCK record source digest mismatch" (record-identity record)))
          (unless (parse-artifact-valid? artifact)
            (error "TCK parse produced an invalid artifact" (record-identity record)))
          (when (and ok? (not (equal? (parse-artifact-roundtrip artifact) source)))
            (error "TCK accepted artifact does not roundtrip" (record-identity record)))
          (let* ((unexpected? (and expected? (not ok?)))
                 (new-unexpected-source?
                  (and unexpected?
                       (not (hash-key? seen-unexpected-sources digest))))
                 (next-failures
                  ;; The pinned corpus currently has fewer than 256 unexpected
                  ;; syntax rejections. Keep every one attributable while
                  ;; bounding the receipt if the upstream corpus grows.
                  (if (and unexpected? (< (length failures) 256))
                    (cons
                     (list
                      (record-identity record)
                      (record-path record)
                      (record-scenario-line record)
                      (record-query-line record)
                      (record-name record)
                      (record-example-id record)
                      (record-source-digest record)
                      (substring source 0 (min 320 (string-length source)))
                      (parse-artifact-ref artifact 'diagnostics))
                     failures)
                    failures)))
            (when new-source? (hash-put! seen-sources digest #t))
            (when new-unexpected-source?
              (hash-put! seen-unexpected-sources digest #t))
            (loop
             (cdr rest)
             (fx+ index 1)
             (fx+ unique-sources (if new-source? 1 0))
             (fx+ unique-unexpected-sources
                  (if new-unexpected-source? 1 0))
             (fx+ accepted (if ok? 1 0))
             (fx+ rejected (if ok? 0 1))
             (fx+ expected-accepted (if (and expected? ok?) 1 0))
             (fx+ expected-rejected (if unexpected? 1 0))
             (fx+ compile-accepted
                  (if (and (equal? outcome "compile-error") ok?) 1 0))
             (fx+ compile-rejected
                  (if (and (equal? outcome "compile-error") (not ok?)) 1 0))
             (fx+ skipped (if skip? 1 0))
             (fx+ unresolved (if unresolved? 1 0))
             (if unexpected?
               (increment (path-category (record-path record)) categories)
               categories)
             (if unexpected? (increment failure-kind failure-kinds)
                 failure-kinds)
             (if unexpected? (increment token-kind token-kinds)
                 token-kinds)
             (if unexpected? (increment token-lexeme token-lexemes)
                 token-lexemes)
             (if unexpected? (increment state states) states)
             (if unexpected? (increment expected expected-terminals)
                 expected-terminals)
             (if unexpected?
               (foldl
                (lambda (site found)
                  (increment (list (car site) (cadr site)) found))
                ambiguity-branch-sites
                diagnostic-branch-sites)
               ambiguity-branch-sites)
             next-failures)))))))
      )

(unless (>= (length (command-line)) 2)
  (error "usage: opencypher-tck-syntax.ss MANIFEST"))
(write (audit (car (reverse (command-line)))))
(newline)
