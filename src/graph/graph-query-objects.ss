;;; -*- Gerbil -*-
;;; POO-native graph query context; language packs supply their own view.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop validate element?)
        (only-in :yuanxu/object-family/interface defpoo-object-family)
        (only-in ./graph-query-types
                 +graph-query-context-kind+
                 GraphQueryContextContract GraphQueryViewContract))
(export GraphQueryContext. make-graph-query-context
        graph-query-context-view graph-query-context-index
        graph-query-context-count)

(def GraphQueryContext. (.ref GraphQueryContextContract 'proto))

(def (make-graph-query-context Row Id Kind source-view)
  (when (element? Id #f) (error "graph identity Type must exclude the no-parent sentinel"))
  ;; Retain the checked view, rather than discarding Function wrappers after a
  ;; boolean shape check. Native Function owns callback arity and record input.
  (let* ((view-type (GraphQueryViewContract Row Id Kind))
         (graph-view (validate view-type source-view))
        (by-id (make-hash-table)))
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
    (.o (:: @ GraphQueryContext.)
        kind: +graph-query-context-kind+
        record-type: Row id-type: Id kind-type: Kind
        view: (cons view-type graph-view) index: by-id
        count: (length (.ref graph-view 'records)))))

(defpoo-object-family
  (accessors
   (graph-query-context-index index)
   (graph-query-context-count count))
  (projections))

(def (graph-query-context-view context) (cdr (.ref context 'view)))
