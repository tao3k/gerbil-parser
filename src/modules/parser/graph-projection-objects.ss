;;; -*- Gerbil -*-
;;; POO-native graph projection declarations; syntax belongs to language packs.

(import (only-in :clan/poo/object .o .ref)
        (only-in :poo-flow-foundation/module-system/types
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?)
        (only-in :poo-flow-foundation/module-system/object-family/interface
                 defpoo-object-family)
        (only-in ./graph-projection-types
                 +graph-projection-kind+ +graph-node-kind+ +graph-field-kind+
                 GraphProjectionContract GraphNodeContract GraphFieldContract))
(export GraphProjection. GraphNode. GraphField.
        make-graph-projection make-graph-node make-graph-field
        graph-projection? graph-projection-nodes
        graph-node-syntax-kind graph-node-category graph-node-label graph-node-fields
        graph-field-token graph-field-name graph-field-mode)

(def GraphProjection. (.ref GraphProjectionContract 'proto))
(def GraphNode. (.ref GraphNodeContract 'proto))
(def GraphField. (.ref GraphFieldContract 'proto))

(def (admit-graph! contract candidate)
  (let (evidence (poo-flow-contract-admit contract candidate #f))
    (unless (poo-flow-validation-evidence-accepted? evidence)
      (error "invalid POO graph projection" (.ref evidence 'diagnostics)))
    candidate))

(def (make-graph-field token name (mode 'append))
  (admit-graph! GraphFieldContract
                (.o (:: @ GraphField.)
                    kind: +graph-field-kind+
                    token-kind: token
                    field-name: name
                    field-mode: mode)))

(def (make-graph-node syntax-kind category label fields)
  (admit-graph! GraphNodeContract
                (.o (:: @ GraphNode.)
                    kind: +graph-node-kind+
                    cst-node-kind: syntax-kind
                    graph-category: category
                    graph-label: label
                    field-rules: fields)))

(def (make-graph-projection nodes)
  (admit-graph! GraphProjectionContract
                (.o (:: @ GraphProjection.)
                    kind: +graph-projection-kind+
                    node-rules: nodes)))

(defpoo-object-family
  (accessors
   (graph-projection-nodes node-rules)
   (graph-node-syntax-kind cst-node-kind)
   (graph-node-category graph-category)
   (graph-node-label graph-label)
   (graph-node-fields field-rules)
   (graph-field-token token-kind)
   (graph-field-name field-name)
   (graph-field-mode field-mode))
  (projections))

(def (graph-projection? value)
  (poo-flow-validation-evidence-accepted?
   (poo-flow-contract-admit GraphProjectionContract value #f)))
