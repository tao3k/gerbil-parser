(import :std/test ../src/compiler/event-fold-consumer-state)
(export fold-consumer-state-test)
(def a '(uint-positive? (state a)))
(def b '(uint-positive? (state b)))
(def hit '(token Hit start end))
(def fold-consumer-state-test
  (test-suite "Consumer-local selector dependency support"
    (test-case "positive and Boolean selectors refine their own consumer"
      (check (fold-consumer-selector-body `((if ,a ((if ,a (,hit) ())) ())))
             => `((if ,a (,hit) ())))
      (check (fold-consumer-selector-body '((if (state enabled) () ((if (state enabled) ((token Wrong start end)) ())))))
             => '((if (state enabled) () ()))))
    (test-case "unrelated consumer writes preserve eligibility"
      (check (fold-consumer-selector-body `((if ,a ((set-uint b (uint 1)) (if ,a (,hit) ())) ())))
             => `((if ,a ((set-uint b (uint 1)) ,hit) ()))))
    (test-case "dependent writes and barriers reject stale reuse"
      (for-each
        (lambda (mutation)
          (let (forms `((if ,a (,mutation (if ,a (,hit) ())) ())))
            (check (fold-consumer-selector-body forms) => forms)))
        '((set-uint a (uint 0)) (opaque-call) (call-source-helper helper start end))))
    (test-case "composite selectors retain event order and independent support"
      (check (fold-consumer-selector-body `((if (and ,a ,b) ((token First start end) (if ,b (,hit) ())) ())))
             => `((if (and ,a ,b) ((token First start end) ,hit) ()))))
    (test-case "branch joins and iterations do not inherit unproven facts"
      (let (forms `((if ,a ((set-uint b (uint 0))) ()) (if ,a (,hit) ())))
        (check (fold-consumer-selector-body forms) => forms))
      (let (forms `((if ,a ((for-line-bytes cursor start end ((if ,a (,hit) ())))) ())))
        (check (fold-consumer-selector-body forms) => forms)))))
