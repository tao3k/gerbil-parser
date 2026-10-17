;;; -*- Gerbil -*-
;;; POO-native graph query context; language packs supply their own view.

(import (only-in :clan/poo/object .o .ref)
        (only-in :core/types
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?)
        (only-in :core/object-family/interface defpoo-object-family)
        (only-in ./graph-query-types
                 +graph-query-context-kind+
                 GraphQueryContextContract graph-query-view?))
(export GraphQueryContext. make-graph-query-context
        graph-query-context-view graph-query-context-index
        graph-query-context-count)

(def GraphQueryContext. (.ref GraphQueryContextContract 'proto))

(def (make-graph-query-context graph-view)
  (unless (graph-query-view? graph-view)
    (error "graph query requires a POO graph view" graph-view))
  (let (by-id (make-hash-table))
    (for-each
     (lambda (record)
       (let (id ((.ref graph-view 'id-of) record))
         (when (hash-get by-id id)
           (error "duplicate graph record id" id))
         (hash-put! by-id id record)))
     (.ref graph-view 'records))
    (for-each
     (lambda (record)
       (let (parent ((.ref graph-view 'parent-of) record))
         (when (and parent (not (hash-get by-id parent)))
           (error "missing graph parent" parent))))
     (.ref graph-view 'records))
    (let* ((candidate
            (.o (:: @ GraphQueryContext.)
                kind: +graph-query-context-kind+
                view: graph-view
                index: by-id
                count: (length (.ref graph-view 'records))))
           (evidence (poo-flow-contract-admit
                      GraphQueryContextContract candidate #f)))
      (unless (poo-flow-validation-evidence-accepted? evidence)
        (error "invalid POO graph query context"
               (.ref evidence 'diagnostics)))
      candidate)))

(defpoo-object-family
  (accessors
   (graph-query-context-view view)
   (graph-query-context-index index)
   (graph-query-context-count count))
  (projections))
