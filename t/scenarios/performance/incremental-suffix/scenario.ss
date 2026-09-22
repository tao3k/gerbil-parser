;;; -*- Gerbil -*-
;;; Full-size incremental LR checkpoint and suffix-convergence workload.

(import :gerbil-parser/languages/arithmetic/v1/parser
        :gerbil-parser/src/runtime/incremental)
(export incremental-suffix-scenario
        incremental-suffix-scenario-pass?)

(def +operand-count+ 100)
(def +source+
  (string-append
   "(" (string-join (make-list +operand-count+ "001") " + ") ")"))
(def +edit-start+ (+ 1 (* 50 6)))
(def +edit+ (make-edit +edit-start+ 3 "002"))
(def +base-artifact+ (parse-arithmetic-v1 +source+))
(def +fresh-artifact+
  (parse-arithmetic-v1 (apply-edit +source+ +edit+)))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (incremental-suffix-scenario)
  (let-values (((artifact receipt)
                (parse-source/incremental
                 arithmetic-parser +source+ +base-artifact+ +edit+)))
    (list
     (cons 'schema "gerbil-parser.incremental-suffix.v1")
     (cons 'operandCount +operand-count+)
     (cons 'freshEquivalent (equal? artifact +fresh-artifact+))
     (cons 'suffixByteDelta (row-ref receipt 'suffixByteDelta))
     (cons 'convergedSuffixTokenCount
           (row-ref receipt 'convergedSuffixTokenCount))
     (cons 'reusedSuffixTokenCount
           (row-ref receipt 'reusedSuffixTokenCount))
     (cons 'relocatedSuffixTokenCount
           (row-ref receipt 'relocatedSuffixTokenCount))
     (cons 'resumedSignificantTokenCount
           (row-ref receipt 'resumedSignificantTokenCount))
     (cons 'remainingSignificantTokenCount
           (row-ref receipt 'remainingSignificantTokenCount)))))

(def (incremental-suffix-scenario-pass? receipt)
  (and (equal? (row-ref receipt 'schema)
               "gerbil-parser.incremental-suffix.v1")
       (= (row-ref receipt 'operandCount) +operand-count+)
       (row-ref receipt 'freshEquivalent)
       (zero? (row-ref receipt 'suffixByteDelta))
       (> (row-ref receipt 'convergedSuffixTokenCount) 90)
       (= (row-ref receipt 'reusedSuffixTokenCount)
          (row-ref receipt 'convergedSuffixTokenCount))
       (zero? (row-ref receipt 'relocatedSuffixTokenCount))
       (> (row-ref receipt 'resumedSignificantTokenCount) 90)
       (< (row-ref receipt 'remainingSignificantTokenCount) 120)))
