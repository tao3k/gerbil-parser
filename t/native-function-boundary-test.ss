;;; -*- Gerbil -*-
;;; Real callbacks cross native Function boundaries, including nested results.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o .cc .ref)
        (only-in :clan/poo/mop validate TypeError?)
        (only-in :gerbil-parser/graph-query-support
                 GraphQueryViewContract make-graph-query-context graph-query-map)
        (only-in :gerbil-parser/src/modules/parser/graph-query-objects graph-query-context-view)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state source-scanner-step)
        (only-in :gerbil-parser/src/runtime/function-types ActionSelectorFactory)
        (only-in :gerbil-parser/src/runtime/token make-token token-kind))
(include "../scripts/test-plan.ss")
(deflanguage-parser-tests native-function-boundary-test "native parser Function boundaries"
  (engine)
    (scenario "graph admission retains checked callbacks and never executes them"
      (sequential ((calls 0)
             (view (.o records: '()
                       id-of: (lambda (record) (set! calls (+ calls 1)) (.ref record 'id))
                       parent-of: (lambda (record) (.ref record 'parent))
                       kind-of: (lambda (record) (.ref record 'kind))))
             (checked (validate GraphQueryViewContract view)))
        (equal calls 0)
        (raises ((.ref checked 'id-of) 42) TypeError?)
        (equal calls 0)
        (equal ((.ref checked 'id-of) (.o id: 'a)) 'a)
        (equal calls 1)
        (raises (validate GraphQueryViewContract (.cc view 'id-of 42)) TypeError?)))
    (scenario "context owns its admitted view and Scheme truth-value predicates"
      (sequential ((record (.o id: 'root parent: #f kind: 'node))
             (raw (.o records: (list record) id-of: (lambda (r) (.ref r 'id))
                      parent-of: (lambda (r) (.ref r 'parent))
                      kind-of: (lambda (r) (.ref r 'kind))))
             (context (make-graph-query-context raw)))
        (raises ((.ref (graph-query-context-view context) 'kind-of) 42) TypeError?)
        (equal (graph-query-map context 'node (lambda (_) '(truthy))) (list record))
        (raises (graph-query-map context 'node 42) TypeError?)))
    (scenario "scanner checks multiple results and preserves progress invariants"
      (sequential ((worker (make-source-scanner "x" 'state
                       (lambda (_source offset context _mode)
                         (values 'word (+ offset 1) context))))
             (state (source-scanner-initial-state worker)))
        (scope-values (((token next) (source-scanner-step worker state 'mode)))
          (equal (token-kind token) 'word)))
      (each (step) ((list (lambda _ (values 42 1 #f))
                         (lambda _ (values 'word -1 #f))
                         (lambda _ (values 'word 1))))
        (scope ((worker (make-source-scanner "x" #f step)))
          (raises (source-scanner-step worker (source-scanner-initial-state worker) 'mode)
                  TypeError?)))
      (scope ((worker (make-source-scanner "x" #f (lambda _ (values 'word 0 #f)))))
        (raises (source-scanner-step worker (source-scanner-initial-state worker) 'mode)
                error?)))
    (scenario "selector factories validate the returned callback before retaining it"
      (sequential ((calls 0)
             (factory (validate ActionSelectorFactory
                        (lambda (_rows _index _fold? _layout?)
                          (lambda (state token) (set! calls (+ calls 1)) state))))
             (selector (factory '#() '#() #f #f)))
        (equal calls 0)
        (equal (selector 2 (make-token 'word "a" 0 1)) 2)
        (raises (selector -1 #f) TypeError?)
        (raises (selector 0 'not-a-token) TypeError?)
        (equal calls 1)
        (raises ((validate ActionSelectorFactory (lambda _ 42)) '#() '#() #f #f)
                TypeError?)))
    (property "compiled conformance owns and executes the boundary Suite"
      (bindings (covered (conformance-covered-test-files)))
      (equal (and (member "t/native-function-boundary-test.ss" covered) #t) #t)))
