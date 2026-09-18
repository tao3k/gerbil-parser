;;; -*- Gerbil -*-
;;; Full-size openCypher repetition workload for LR complexity regression.

(import (only-in :std/srfi/13 string-join)
        :gerbil-parser/languages/cypher/opencypher-2024-1/parser
        :gerbil-parser/src/runtime/artifact)
(export opencypher-large-query-scenario
        opencypher-large-query-scenario-pass?)

(def +clause-count+ 100)
(def +source+
  (string-join
   (make-list +clause-count+ "CREATE (n:Node {value: null})")
   "\n"))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (opencypher-large-query-scenario)
  (let (artifact (parse-opencypher-2024-1 +source+))
    (list
     (cons 'schema "gerbil-parser.opencypher-large-query.v1")
     (cons 'clauseCount +clause-count+)
     (cons 'sourceByteLength (string-length +source+))
     (cons 'status (parse-artifact-status artifact))
     (cons 'valid (parse-artifact-valid? artifact))
     (cons 'roundtrip
           (and (parse-artifact-success? artifact)
                (equal? (parse-artifact-roundtrip artifact) +source+))))))

(def (opencypher-large-query-scenario-pass? receipt)
  (and (equal? (row-ref receipt 'schema)
               "gerbil-parser.opencypher-large-query.v1")
       (= (row-ref receipt 'clauseCount) +clause-count+)
       (eq? (row-ref receipt 'status) 'accepted)
       (row-ref receipt 'valid)
       (row-ref receipt 'roundtrip)))
