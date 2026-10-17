;;; -*- Gerbil -*-
;;; Language-neutral POO admission for projected graph query contexts.

(import (only-in :clan/poo/object .o .ref .slot? object?)
        (only-in :clan/poo/mop define-type)
        (only-in :core/types
                 PooFlowContract. PooFlowNativeObjectContract.
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?
                 poo-flow-classification-evidence))
(export +graph-query-context-kind+
        GraphQueryViewContract GraphQueryContextContract
        graph-query-view? graph-query-context?)

(def +graph-query-context-kind+ 'gerbil-parser-graph-query-context)

(def (classify identity predicate candidate context)
  (let (accepted? (predicate candidate))
    (poo-flow-classification-evidence
     identity candidate accepted?
     (if accepted? '() (list (list 'expected identity))) context)))

(def (graph-query-view-shape? value)
  (and (object? value)
       (.slot? value 'records)
       (list? (.ref value 'records))
       (andmap (lambda (slot)
                 (and (.slot? value slot)
                      (procedure? (.ref value slot))))
               '(id-of parent-of kind-of))))

(define-type (GraphQueryViewContract @ PooFlowContract.)
  identity: 'gerbil-parser/graph-query-view
  .classify: (lambda (candidate context)
               (classify 'gerbil-parser/graph-query-view
                         graph-query-view-shape? candidate context)))

(define-type (GraphQueryContextKind @ PooFlowContract.)
  identity: 'gerbil-parser/graph-query-context-kind
  .classify: (lambda (candidate context)
               (classify 'gerbil-parser/graph-query-context-kind
                         (lambda (value)
                           (eq? value +graph-query-context-kind+))
                         candidate context)))

(define-type (GraphQueryIndex @ PooFlowContract.)
  identity: 'gerbil-parser/graph-query-index
  .classify: (lambda (candidate context)
               (classify 'gerbil-parser/graph-query-index
                         hash-table? candidate context)))

(define-type (GraphQueryCount @ PooFlowContract.)
  identity: 'gerbil-parser/graph-query-count
  .classify: (lambda (candidate context)
               (classify 'gerbil-parser/graph-query-count
                         (lambda (value)
                           (and (exact-integer? value) (>= value 0)))
                         candidate context)))

(define-type (GraphQueryContextContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/graph-query-context
  proto: (.o)
  responsibilities:
  (.o kind: GraphQueryContextKind
      view: GraphQueryViewContract
      index: GraphQueryIndex
      count: GraphQueryCount))

(def (admitted? contract candidate)
  (poo-flow-validation-evidence-accepted?
   (poo-flow-contract-admit contract candidate #f)))

(def (graph-query-view? value)
  (admitted? GraphQueryViewContract value))

(def (graph-query-context? value)
  (admitted? GraphQueryContextContract value))
