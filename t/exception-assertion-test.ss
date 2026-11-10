(import :gerbil-parser/language-test-support)

(deflanguage-parser-tests exception-assertion-test "Exception assertion outcomes"
  (engine)
    (scenario "normal returns cannot satisfy an exception assertion"
      (each (produce) ((list (lambda () 42) (lambda () #f)
                             (lambda () (values)) (lambda () (values 1 2))))
        (equal (parser-exception-matches? produce true) #f)))
    (scenario "raised values include false without being confused with a normal return"
      (each (failure) ('(#f problem 42))
        (equal (parser-exception-matches? (lambda () (raise failure)) true) #t)))
    (scenario "predicates inspect the subject exception and the subject runs once"
      (scope ((calls 0))
        (equal (parser-exception-matches?
                (lambda () (set! calls (1+ calls)) (raise 'problem)) symbol?) #t)
        (equal calls 1))
      (equal (parser-exception-matches? (lambda () (raise 42)) symbol?) #f)))
