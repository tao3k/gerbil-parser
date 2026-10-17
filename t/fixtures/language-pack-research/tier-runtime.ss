#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Runtime controls consume the repository's canonical lossless events.
(import "tier-language"
        (only-in "../../../languages/arithmetic/grammar" arithmetic-parser)
        (only-in "../../../src/runtime/parser" parse-source)
        (only-in "../../../src/runtime/artifact"
                 parse-artifact-success? parse-artifact-roundtrip parse-artifact-events))

(def checked 0)
(def (runtime-check label actual expected)
  (unless (equal? actual expected)
    (error "tier runtime mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "TIER-RUNTIME-CASE-OK: ") (display label) (newline) (force-output))

(def (accepted source)
  (let (artifact (parse-source tier-study-parser source))
    (runtime-check (list 'accepted source) (parse-artifact-success? artifact) #t)
    (runtime-check (list 'roundtrip source) (parse-artifact-roundtrip artifact) source)
    artifact))

;;; Compare complete events: node ids/kinds/order, fields, tokens, and spans.
(for-each
 (lambda (source)
   (let ((candidate (accepted source))
         (baseline (parse-source arithmetic-parser source)))
     (runtime-check (list 'baseline-accepted source)
       (parse-artifact-success? baseline) #t)
     (runtime-check (list 'arithmetic-event-parity source)
       (parse-artifact-events candidate) (parse-artifact-events baseline))))
 '("a" "42" "a-b-c" "1-2-3" "a - b - c" "a+b*c" "(a+b)*c" "-a*b" "a+-b" "--a"
   "a / b - c" " \n a + b \t"))

;;; Exact binary node boundaries distinguish left and right nesting.
(def (binary-boundaries artifact)
  (map (lambda (event)
         (list (vector-ref event 0) (vector-ref event 3)))
       (filter (lambda (event)
                 (and (memq (vector-ref event 0) '(start-node finish-node))
                      (eq? (vector-ref event 2) 'Expression)))
               (parse-artifact-events artifact))))
(runtime-check 'left-nesting-and-spans
  (binary-boundaries (accepted "1-2-3"))
  '((start-node 0) (start-node 0) (finish-node 3) (finish-node 5)))
(runtime-check 'right-nesting-and-spans
  (binary-boundaries (accepted "a**b**c"))
  '((start-node 0) (start-node 3) (finish-node 7) (finish-node 7)))
(runtime-check 'hyphenated-identifier-is-not-subtraction
  (binary-boundaries (accepted "a-b-c")) '())
(runtime-check 'base-has-no-binary-wrapper
  (binary-boundaries (accepted "a")) '())

(for-each
 (lambda (source)
   (runtime-check (list 'rejected source)
     (parse-artifact-success? (parse-source tier-study-parser source)) #f))
 '("" "a+" "*a" "a b" "(a+b"))
(display "TIER-RUNTIME-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
;;; This is a terminal research driver; successful completion must terminate.
(exit 0)
