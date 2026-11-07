;;; Native prototypes extend the engine's actual preparation, not a wrapper API.
(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .o)
        (only-in :yuanxu/poo-clos/interface .defmethod)
        (only-in :gerbil-parser/src/runtime/reduction-plan RecognitionSemantics. EventSemantics.
                 select-lr-action-factory select-lr-action-factory$poo-clos-generic
                 prepare-lr-reduction-plans reduce-lr-stack-plan)
        (only-in :gerbil-parser/src/runtime/reduce make-recognition-alias-kernel)
        (only-in :gerbil-parser/src/runtime/recognition make-recognition-child
                 recognition-child-value recognition-node-kind recognition-node-start recognition-node-end)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-prepare prepare-lr-semantics lr-parse/prepared
                 lr-runtime-for-current-semantic-backend)
        (only-in :gerbil-parser/src/runtime/lr-parser/state current-lr-event-program-enabled?))
(def DecoratedBackend. (.o (:: @ RecognitionSemantics.)))
(def TraceMixin. (.o trace-label: 'example))
(def ComposedBackend. (.o (:: @ [DecoratedBackend. TraceMixin.])))
(def RevisionBackend. (.o (:: @ RecognitionSemantics.)))
(def selected-count 0)
(.defmethod (select-lr-action-factory
              (backend (prototype DecoratedBackend.)) (kind (eql 'alias)))
  (set! selected-count (+ selected-count 1))
  (lambda (_name) (make-recognition-alias-kernel 'Decorated)))
(def model-production '(0 root ((marked (terminal token word) ((alias Base)))) pass #f))
(def (model-spec)
  (list (cons 'productions (list model-production)) (cons 'declarations '(root))
        (cons 'actions '#((((terminal token word) shift 1 #f))
                         (((terminal eof) reduce 0)) (((terminal eof) accept))))
        (cons 'gotos '#(((root . 2)) () ())) (cons 'case-insensitive? #f)))
(def (plan-root plan)
  (recognition-child-value
    (car (reduce-lr-stack-plan (vector-ref plan 0)
           (list (list (make-recognition-child #f (make-token 'word "x" 2 3)))) 9))))
(deflanguage-parser-tests semantic-backend-test "prototype-composed LR semantic preparation"
  ;; The native Gerbil harness owns Suite/Case execution order.
  (engine)
    (scenario "native inheritance and a neutral mixin change actual node semantics"
      (scope ((node (plan-root (prepare-lr-reduction-plans ComposedBackend. (vector model-production)))))
        (equal (recognition-node-kind node) 'Decorated)
        (equal (recognition-node-start node) 2)
        (equal (recognition-node-end node) 3)))
    (scenario "method resolution happens during preparation and not reductions"
      (sequential ()
        (act (set! selected-count 0))
        (scope ((plan (prepare-lr-reduction-plans ComposedBackend. (vector model-production))))
          (equal selected-count 1)
          (equal (recognition-node-kind (plan-root plan)) 'Decorated)
          (equal (recognition-node-kind (plan-root plan)) 'Decorated)
          (equal selected-count 1))))
    (scenario "one prepared selection serves multiple grammar runtimes"
      (sequential ()
        (act (set! selected-count 0))
        (scope ((semantics (prepare-lr-semantics ComposedBackend.)))
          (equal selected-count 1)
          (scope ((left (lr-prepare (model-spec) semantics))
                  (right (lr-prepare (model-spec) semantics)))
            (equal (eq? left right) #f)
            (equal selected-count 1)))))
    (scenario "runtime selection consumes the prototype and preserves custom semantics"
      (sequential ((runtime (lr-prepare (model-spec) ComposedBackend.)))
        (scope-values (((root rest) (lr-parse/prepared runtime (list (make-token 'word "x" 2 3)))))
          (equal (recognition-node-kind root) 'Decorated)
          (equal rest '()))
        (equal (parameterize ((current-lr-event-program-enabled? #t))
                 (eq? (lr-runtime-for-current-semantic-backend runtime) runtime)) #t)))
    (scenario "new method contributions affect new plans and retain old callables"
      (sequential ((old (prepare-lr-reduction-plans RevisionBackend. (vector model-production))))
        (equal (recognition-node-kind (plan-root old)) 'Base)
        (act (.defmethod (select-lr-action-factory
                           (backend (prototype RevisionBackend.)) (kind (eql 'alias)))
               (lambda (_name) (make-recognition-alias-kernel 'Revised))))
        (scope ((fresh (prepare-lr-reduction-plans RevisionBackend. (vector model-production))))
          (equal (recognition-node-kind (plan-root fresh)) 'Revised)
          (equal (recognition-node-kind (plan-root old)) 'Base))))
    (scenario "missing families and the wrong representation fail before use"
      (raises (prepare-lr-reduction-plans (.o) '#()) true)
      (raises (prepare-lr-reduction-plans 'unknown '#()) true)
      (raises (lr-prepare (model-spec) EventSemantics.) true))
    (scenario "legacy symbol selection preserves builtin semantics"
      (scope ((node (plan-root (prepare-lr-reduction-plans 'recognition (vector model-production)))))
        (equal (recognition-node-kind node) 'Base))))
