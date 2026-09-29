;;; -*- Gerbil -*-
;;; Pure, language-neutral selection over POO projected graph views.

(import (only-in :clan/poo/object .ref)
        (only-in ./graph-query-objects
                 graph-query-context-view graph-query-context-index
                 graph-query-context-count))
(export graph-query-map graph-query-lineage? graph-query-select)

(def (graph-query-map context kind predicate)
  (let (view (graph-query-context-view context))
    (filter
     (lambda (record)
       (and (equal? ((.ref view 'kind-of) record) kind)
            (predicate record)))
     (.ref view 'records))))

(def (graph-query-lineage? context ancestor descendant)
  (let* ((view (graph-query-context-view context))
         (by-id (graph-query-context-index context))
         (parent-of (.ref view 'parent-of)))
    (let loop ((id descendant) (remaining (graph-query-context-count context)))
      (and (> remaining 0)
           (let (record (hash-get by-id id))
             (and record
                  (let (parent (parent-of record))
                    (and parent
                         (or (equal? parent ancestor)
                             (loop parent (- remaining 1)))))))))))

(def (graph-query-select context kind scope-id relation targets predicate)
  (unless (memq relation '(any at child-of descendant-of))
    (error "unknown graph relation" relation))
  (unless (list? targets)
    (error "graph query targets must be a list" targets))
  (let* ((view (graph-query-context-view context))
         (id-of (.ref view 'id-of))
         (parent-of (.ref view 'parent-of)))
    (graph-query-map
     context kind
     (lambda (record)
       (let (id (id-of record))
         (and (or (equal? id scope-id)
                  (graph-query-lineage? context scope-id id))
              (case relation
                ((any) #t)
                ((at) (member id targets))
                ((child-of) (member (parent-of record) targets))
                ((descendant-of)
                 (ormap (lambda (target)
                          (graph-query-lineage? context target id))
                        targets)))
              (predicate record)))))))
