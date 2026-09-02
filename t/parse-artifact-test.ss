#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/reference/arithmetic-v1)

(def (artifact-replace artifact key value)
  (map (lambda (row)
         (if (eq? (car row) key) (cons key value) row))
       artifact))

(def (remove-token-id events rejected-id)
  (let loop ((rest events) (found '()))
    (cond
     ((null? rest) (reverse found))
     ((and (token-event? (car rest))
           (= (token-event-id (car rest)) rejected-id))
      (loop (cdr rest) found))
     (else (loop (cdr rest) (cons (car rest) found))))))

(def parse-artifact-tests
  (test-suite "ParseArtifact contract"
    (test-case "identity and event replay are deterministic"
      (let* ((source "alpha + (2 * beta)")
             (first (parse-source arithmetic-parser source))
             (second (parse-source arithmetic-parser source))
             (first-cst (parse-artifact->cst first))
             (second-cst (parse-artifact->cst second)))
        (check first => second)
        (check (parse-artifact-ref first 'grammarDigest)
               => (parse-artifact-ref second 'grammarDigest))
        (check (parse-artifact-ref first 'sourceDigest)
               => (sha256-text source))
        (check first-cst => second-cst)))
    (test-case "a missing token event fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (corrupt
              (artifact-replace
               artifact 'events
               (remove-token-id (parse-artifact-events artifact) 2))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "a duplicate token identity fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (events (parse-artifact-events artifact))
             (first-token
              (let loop ((rest events))
                (if (token-event? (car rest))
                  (car rest)
                  (loop (cdr rest)))))
             (corrupt
              (artifact-replace artifact 'events
                                (cons first-token events))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "an unbalanced root finish fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (events (parse-artifact-events artifact))
             (corrupt-events
              (reverse
               (cons (vector 'finish-node 0 'SourceFile 4)
                     (cdr (reverse events)))))
             (corrupt
              (artifact-replace artifact 'events corrupt-events)))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "source identity cannot drift from canonical token events"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (corrupt
              (artifact-replace artifact 'sourceDigest
                                (sha256-text "different source"))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "rejected artifacts expose no structural event"
      (let ((artifact (parse-source arithmetic-parser "1 + @")))
        (check (parse-artifact-status artifact) => 'rejected)
        (check (parse-artifact-valid? artifact) => #t)
        (check (let loop ((rest (parse-artifact-events artifact)))
                 (or (null? rest)
                     (and (token-event? (car rest))
                          (loop (cdr rest)))))
               => #t)))))

(run-tests! parse-artifact-tests)
