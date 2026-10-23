;;; -*- Gerbil -*-
;;; POO admission for language-owned CST-to-graph projections.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type)
        (only-in :yuanxu/types
                 YuanxuContract. YuanxuObjectContract.
                 yuanxu-contract-admit
                 yuanxu-validation-evidence-accepted?
                 yuanxu-classification-evidence)
        (only-in ./types ParserSymbol))
(export +graph-projection-kind+ +graph-node-kind+ +graph-field-kind+
        GraphProjectionContract GraphNodeContract GraphFieldContract)

(def +graph-projection-kind+ 'gerbil-parser-graph-projection)
(def +graph-node-kind+ 'gerbil-parser-graph-node)
(def +graph-field-kind+ 'gerbil-parser-graph-field)

(def (empty-prototype) (.o))

(def (graph-classify identity predicate candidate context)
  (let (accepted? (predicate candidate))
    (yuanxu-classification-evidence
     identity candidate accepted?
     (if accepted? '() (list (list 'expected identity))) context)))

(def (kind-contract identity expected)
  (lambda (candidate context)
    (graph-classify identity (lambda (value) (eq? value expected))
              candidate context)))

(define-type (GraphProjectionKind @ YuanxuContract.)
  identity: 'gerbil-parser/graph-projection-kind
  .classify: (kind-contract 'gerbil-parser/graph-projection-kind
                             +graph-projection-kind+))
(define-type (GraphNodeKind @ YuanxuContract.)
  identity: 'gerbil-parser/graph-node-kind
  .classify: (kind-contract 'gerbil-parser/graph-node-kind
                             +graph-node-kind+))
(define-type (GraphFieldKind @ YuanxuContract.)
  identity: 'gerbil-parser/graph-field-kind
  .classify: (kind-contract 'gerbil-parser/graph-field-kind
                             +graph-field-kind+))

(define-type (GraphLabel @ YuanxuContract.)
  identity: 'gerbil-parser/graph-label
  .classify: (lambda (candidate context)
               (graph-classify 'gerbil-parser/graph-label
                         (lambda (value)
                           (and (string? value) (> (string-length value) 0)))
                         candidate context)))

(define-type (GraphFieldMode @ YuanxuContract.)
  identity: 'gerbil-parser/graph-field-mode
  .classify: (lambda (candidate context)
               (graph-classify 'gerbil-parser/graph-field-mode
                         (lambda (value)
                           (memq value '(append each append-or-empty node-text each-node-text)))
                         candidate context)))

(define-type (GraphFieldContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/graph-field
  proto: (empty-prototype)
  responsibilities:
  (.o kind: GraphFieldKind
      token-kind: ParserSymbol
      field-name: GraphLabel
      field-mode: GraphFieldMode))

(def (valid-fields? fields)
  (and (list? fields)
       (andmap (lambda (field)
                 (yuanxu-validation-evidence-accepted?
                  (yuanxu-contract-admit GraphFieldContract field #f)))
               fields)))

(define-type (GraphFields @ YuanxuContract.)
  identity: 'gerbil-parser/graph-fields
  .classify: (lambda (candidate context)
               (graph-classify 'gerbil-parser/graph-fields
                         valid-fields? candidate context)))

(define-type (GraphNodeContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/graph-node
  proto: (empty-prototype)
  responsibilities:
  (.o kind: GraphNodeKind
      cst-node-kind: ParserSymbol
      graph-category: GraphLabel
      graph-label: GraphLabel
      field-rules: GraphFields))

(def (valid-nodes? nodes)
  (and (list? nodes)
       (not (null? nodes))
       (andmap (lambda (node)
                 (yuanxu-validation-evidence-accepted?
                  (yuanxu-contract-admit GraphNodeContract node #f)))
               nodes)
       (let loop ((rest nodes) (seen '()))
         (if (null? rest)
           #t
           (let (kind (.ref (car rest) 'cst-node-kind))
             (and (not (memq kind seen))
                  (loop (cdr rest) (cons kind seen))))))))

(define-type (GraphNodes @ YuanxuContract.)
  identity: 'gerbil-parser/graph-nodes
  .classify: (lambda (candidate context)
               (graph-classify 'gerbil-parser/graph-nodes
                         valid-nodes? candidate context)))

(define-type (GraphProjectionContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/graph-projection
  proto: (empty-prototype)
  responsibilities:
  (.o kind: GraphProjectionKind
      node-rules: GraphNodes))
