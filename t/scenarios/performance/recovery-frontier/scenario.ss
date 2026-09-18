;;; -*- Gerbil -*-
;;; Full-size deterministic failure-frontier recovery workload.

(import (only-in :std/srfi/13 string-join)
        :gerbil-parser/languages/arithmetic/v1/parser
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/recovery)
(export recovery-frontier-scenario
        recovery-frontier-scenario-pass?)

(def +operand-count+ 100)
(def +source+
  (string-append
   "("
   (string-join (make-list +operand-count+ "1") " + ")))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (recovery-frontier-scenario)
  (let-values (((artifact receipt)
                (parse-source/recover arithmetic-parser +source+)))
    (list
     (cons 'schema "gerbil-parser.recovery-frontier.v1")
     (cons 'operandCount +operand-count+)
     (cons 'sourceByteLength (string-length +source+))
     (cons 'publicationStatus
           (parse-artifact-ref artifact 'status))
     (cons 'outcome (row-ref receipt 'outcome))
     (cons 'operationKind
           (alet (operations (row-ref receipt 'operations))
             (and (pair? operations)
                  (row-ref (car operations) 'kind))))
     (cons 'frontierState (row-ref receipt 'frontierState))
     (cons 'expectedTerminalCount
           (length (row-ref receipt 'frontierExpectedTerminals)))
     (cons 'reusedPrefixTokenCount
           (row-ref receipt 'reusedPrefixTokenCount))
     (cons 'attempts (row-ref receipt 'attempts)))))

(def (recovery-frontier-scenario-pass? receipt)
  (and (equal? (row-ref receipt 'schema)
               "gerbil-parser.recovery-frontier.v1")
       (= (row-ref receipt 'operandCount) +operand-count+)
       (eq? (row-ref receipt 'publicationStatus) 'rejected)
       (eq? (row-ref receipt 'outcome) 'candidate)
       (eq? (row-ref receipt 'operationKind) 'MISSING)
       (integer? (row-ref receipt 'frontierState))
       (>= (row-ref receipt 'reusedPrefixTokenCount) 199)
       (<= (row-ref receipt 'attempts)
           (row-ref receipt 'expectedTerminalCount))))
