(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-consumer-state)

(def a '(uint-positive? (state a)))
(def b '(uint-positive? (state b)))
(def hit '(token Hit start end))
(deflanguage-parser-tests fold-consumer-state-test "Consumer-local selector dependency support"
  (engine)
    (property "positive and Boolean selectors refine their own consumer"
      (bindings)
      (equal (fold-consumer-selector-body `((if ,a ((if ,a (,hit) ())) ())))
              `((if ,a (,hit) ())))
      (equal (fold-consumer-selector-body '((if (state enabled) () ((if (state enabled) ((token Wrong start end)) ())))))
              '((if (state enabled) () ()))))
    (property "unrelated consumer writes preserve eligibility"
      (bindings)
      (equal (fold-consumer-selector-body `((if ,a ((set-uint b (uint 1)) (if ,a (,hit) ())) ())))
              `((if ,a ((set-uint b (uint 1)) ,hit) ()))))
    (scenario "dependent writes and barriers reject stale reuse"
      (each (mutation) ('((set-uint a (uint 0)) (opaque-call) (call-source-helper helper start end)))
          (scope ((forms `((if ,a (,mutation (if ,a (,hit) ())) ()))))
            (equal (fold-consumer-selector-body forms)  forms))))
    (property "composite selectors retain event order and independent support"
      (bindings)
      (equal (fold-consumer-selector-body `((if (and ,a ,b) ((token First start end) (if ,b (,hit) ())) ())))
              `((if (and ,a ,b) ((token First start end) ,hit) ()))))
    (scenario "branch joins and iterations do not inherit unproven facts"
      (scope ((forms `((if ,a ((set-uint b (uint 0))) ()) (if ,a (,hit) ()))))
        (equal (fold-consumer-selector-body forms)  forms))
      (scope ((forms `((if ,a ((for-line-bytes cursor start end ((if ,a (,hit) ())))) ()))))
        (equal (fold-consumer-selector-body forms)  forms))))
