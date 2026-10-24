;;; -*- Gerbil -*-
;;; Real callbacks cross native Function boundaries, including nested results.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref)
        (only-in :clan/poo/mop validate TypeError?)
        (only-in :gerbil-parser/graph-query-support
                 GraphQueryViewContract make-graph-query-context graph-query-map)
        (only-in :gerbil-parser/src/modules/parser/graph-query-objects graph-query-context-view)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state source-scanner-step)
        (only-in :gerbil-parser/src/runtime/function-types ActionSelectorFactory)
        (only-in :gerbil-parser/src/runtime/token make-token token-kind))
(export native-function-boundary-test)

(def native-function-boundary-test
  (test-suite "native parser Function boundaries"
    (test-case "graph admission retains checked callbacks and never executes them"
      (let* ((calls 0)
             (view (.o records: '()
                       id-of: (lambda (record) (set! calls (+ calls 1)) (.ref record 'id))
                       parent-of: (lambda (record) (.ref record 'parent))
                       kind-of: (lambda (record) (.ref record 'kind))))
             (checked (validate GraphQueryViewContract view)))
        (check-equal? calls 0)
        (check-exception ((.ref checked 'id-of) 42) TypeError?)
        (check-equal? calls 0)
        (check-equal? ((.ref checked 'id-of) (.o id: 'a)) 'a)
        (check-equal? calls 1)
        (check-exception (validate GraphQueryViewContract (.cc view 'id-of 42)) TypeError?)))
    (test-case "context owns its admitted view and Scheme truth-value predicates"
      (let* ((record (.o id: 'root parent: #f kind: 'node))
             (raw (.o records: (list record) id-of: (lambda (r) (.ref r 'id))
                      parent-of: (lambda (r) (.ref r 'parent))
                      kind-of: (lambda (r) (.ref r 'kind))))
             (context (make-graph-query-context raw)))
        (check-exception ((.ref (graph-query-context-view context) 'kind-of) 42) TypeError?)
        (check-equal? (graph-query-map context 'node (lambda (_) '(truthy))) (list record))
        (check-exception (graph-query-map context 'node 42) TypeError?)))
    (test-case "scanner checks multiple results and preserves progress invariants"
      (let* ((worker (make-source-scanner "x" 'state
                       (lambda (_source offset context _mode)
                         (values 'word (+ offset 1) context))))
             (state (source-scanner-initial-state worker)))
        (let-values (((token next) (source-scanner-step worker state 'mode)))
          (check-equal? (token-kind token) 'word)))
      (for-each
       (lambda (step)
         (let (worker (make-source-scanner "x" #f step))
           (check-exception (source-scanner-step worker (source-scanner-initial-state worker) 'mode)
                            TypeError?)))
       (list (lambda _ (values 42 1 #f))
             (lambda _ (values 'word -1 #f))
             (lambda _ (values 'word 1))))
      (let (worker (make-source-scanner "x" #f (lambda _ (values 'word 0 #f))))
        (check-exception (source-scanner-step worker (source-scanner-initial-state worker) 'mode)
                         error?)))
    (test-case "selector factories validate the returned callback before retaining it"
      (let* ((calls 0)
             (factory (validate ActionSelectorFactory
                        (lambda (_rows _index _fold? _layout?)
                          (lambda (state token) (set! calls (+ calls 1)) state))))
             (selector (factory '#() '#() #f #f)))
        (check-equal? calls 0)
        (check-equal? (selector 2 (make-token 'word "a" 0 1)) 2)
        (check-exception (selector -1 #f) TypeError?)
        (check-exception (selector 0 'not-a-token) TypeError?)
        (check-equal? calls 1)
        (check-exception ((validate ActionSelectorFactory (lambda _ 42)) '#() '#() #f #f)
                         TypeError?)))))
