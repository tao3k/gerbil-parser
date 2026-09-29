;;; -*- Gerbil -*-
;;; Generic POO graph query semantics are independent of Org labels.

(import (only-in :std/test check test-case test-suite)
        (only-in :clan/poo/object .o .ref)
        (only-in :gerbil-parser/graph-query-support
                 graph-query-context? make-graph-query-context
                 graph-query-map graph-query-lineage? graph-query-select))
(export graph-query-test)

(def (view source-records)
  (.o records: source-records
      id-of: (lambda (record) (.ref record 'id))
      parent-of: (lambda (record) (.ref record 'parent))
      kind-of: (lambda (record) (.ref record 'kind))))

(def (ids records)
  (map (lambda (record) (.ref record 'id)) records))

(def graph-query-test
  (test-suite "POO graph query context"
    (test-case "scope and relations accept non-preorder language views"
      (let (context
            (make-graph-query-context
             (view (list (.o id: 0 parent: #f kind: "root")
                         (.o id: 1 parent: 0 kind: "section")
                         (.o id: 2 parent: 1 kind: "item")
                         (.o id: 3 parent: 0 kind: "section")
                         (.o id: 4 parent: 1 kind: "item")))))
        (check (graph-query-context? context) => #t)
        (check (ids (graph-query-map context "item" (lambda (_) #t)))
               => '(2 4))
        (check (graph-query-lineage? context 1 4) => #t)
        (check (graph-query-lineage? context 3 4) => #f)
        (check (ids (graph-query-select context "item" 1 'child-of '(1)
                                        (lambda (_) #t)))
               => '(2 4))
        (check (ids (graph-query-select context "item" 0 'descendant-of
                                        '(1 3) (lambda (_) #t)))
               => '(2 4))
        (check (ids (graph-query-select context "item" 0 'any '()
                                        (lambda (record)
                                          (= (.ref record 'id) 4))))
               => '(4))))
    (test-case "invalid graph ancestry is rejected at context admission"
      (check
       (with-catch
        (lambda (_) #t)
        (lambda ()
          (make-graph-query-context
           (view (list (.o id: 0 parent: #f kind: "root")
                       (.o id: 1 parent: 8 kind: "item"))))
          #f))
       => #t))))
