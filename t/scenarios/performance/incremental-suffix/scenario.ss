;;; -*- Gerbil -*-
;;; Full-size incremental LR checkpoint and mode-certified suffix workload.

(import :gerbil-parser/languages/arithmetic/v1/parser
        :gerbil-parser/src/runtime/incremental)
(export incremental-suffix-scenario
        incremental-suffix-scenario-pass?)

(def +operand-count+ 100)
(def +source+
  (string-append
   "(" (string-join (make-list +operand-count+ "001") " + ") ")"))
(def +edit-start+ (+ 1 (* 50 6)))
(def +edit+ (make-edit +edit-start+ 3 "0+2"))
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
     (cons 'reusedSignificantTokenCount
           (row-ref receipt 'reusedSignificantTokenCount))
     (cons 'remainingSignificantTokenCount
           (row-ref receipt 'remainingSignificantTokenCount)))))

(def (incremental-suffix-scenario-pass? receipt)
  (let ((resumed (row-ref receipt 'resumedSignificantTokenCount))
        (reused (row-ref receipt 'reusedSignificantTokenCount))
        (remaining (row-ref receipt 'remainingSignificantTokenCount)))
  (and (equal? (row-ref receipt 'schema)
               "gerbil-parser.incremental-suffix.v1")
       (= (row-ref receipt 'operandCount) +operand-count+)
       (row-ref receipt 'freshEquivalent)
       (zero? (row-ref receipt 'suffixByteDelta))
       (> (row-ref receipt 'convergedSuffixTokenCount) 90)
       (= (row-ref receipt 'reusedSuffixTokenCount)
          (row-ref receipt 'convergedSuffixTokenCount))
       (zero? (row-ref receipt 'relocatedSuffixTokenCount))
       (integer? resumed) (>= resumed 0)
       (integer? reused) (>= reused 0)
       (integer? remaining) (>= remaining 0)
       (> (+ resumed reused) 90)
       (= (+ resumed reused remaining) (+ (* 2 +operand-count+) 1))
       (< remaining 120))))
