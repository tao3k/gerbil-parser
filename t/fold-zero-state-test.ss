(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-zero-state)

(def guard '(uint-positive? (state mode)))
(def branch `(if ,guard ((set-uint mode (uint 2))) ()))
(deflanguage-parser-tests fold-zero-state-test "Guarded zero-state plan"
  (engine)
    (property "guard support excludes payload in an eliminated consumer"
      (bindings (forms '((if (uint-positive? (state mode))
                          ((if (uint-equal? (state payload) (uint 0))
                               ((set-uint payload (uint 3))) ())
                           (set-uint mode (uint 2))) ()))) (plan (fold-zero-state-support forms #t)))
      (equal (vector-ref plan 0)  '(mode))
      (equal (vector-ref plan 1)  '())
      (equal (fold-zero-state-plan forms #t '())  #f))
    (scenario "consumer selectors refine only their local unsigned support"
      (sequential ((other '(if (uint-positive? (state other)) ((token Other start end)) ()))
             (forms `(if ,guard ((token Active start end))
                         ((if ,guard ((token Impossible start end)) ()) ,other))))
        (equal (fold-consumer-state-body (list forms))
                `((if ,guard ((token Active start end)) (,other)))))
      (equal (fold-consumer-state-body `((set-uint mode (uint 0)) ,branch))
              '((set-uint mode (uint 0)))))
    (scenario "consumer writes barriers and joins preserve ordered observations"
      (scope ((forms `((if ,guard ()
                         ((set-uint mode (uint 1)) ,branch)))))
        (equal (fold-consumer-state-body forms)  forms))
      (scope ((forms `((if ,guard () ((opaque-call) ,branch)))))
        (equal (fold-consumer-state-body forms)  forms))
      (scope ((forms `((if (bool-state enabled) ((set-uint mode (uint 0))) ()) ,branch)))
        (equal (fold-consumer-state-body forms)  forms)))
    (scenario "idle guards simplify only behind external zero guard"
      (scope ((plan (fold-zero-state-plan (list branch))))
        (equal (vector-ref plan 0)  '(mode))
        (equal (vector-ref plan 1)  '()))
      (sequential ((configuration '(if (uint-positive? (state policy)) () ()))
             (plan (fold-zero-state-plan (list configuration branch))))
        (equal (vector-ref plan 0)  '(mode))
        (equal (vector-ref plan 1)  (list configuration))))
    (scenario "writes and opaque effects invalidate facts"
      (scope ((plan (fold-ordinary-state-plan
                   '((if (line-byte-equal? (line-index cursor) 99) ((token Text start end)) ())) 'cursor)))
        (equal (vector-ref plan 0)  '())
        (equal (vector-ref plan 1)  '(99))
        (equal (vector-ref plan 2)  '()))
      (scope ((plan (fold-zero-state-plan `((token Text start end) ,branch))))
        (equal (vector-ref plan 1)  '((token Text start end))))
      (equal (fold-zero-state-plan `((set-uint mode (uint 1)) ,branch))  #f)
      (equal (fold-zero-state-plan `((opaque-call) ,branch))  #f))
    (property "unknown branch joins preserve only common facts"
      (bindings (forms `((if (bool-state enabled) ((set-uint mode (uint 1))) ()) ,branch)) (plan (fold-zero-state-plan forms)))
      (equal plan  #f))
    (property "unknown predicate effects retain ordered guards"
      (bindings)
      (equal (fold-zero-state-plan `((if (and (opaque-predicate) ,guard) () ())))  #f)
      (equal (fold-zero-state-plan `((if (opaque-predicate) (,branch) ())))  #f))
    (property "local join binding shadows external facts"
      (bindings)
      (equal (fold-zero-state-plan `((join-once mode (,branch) ())))  #f)))
