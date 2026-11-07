;;; Native materialization and shared scratch controls use the parser test DSL.
(import :gerbil-parser/language-test-support
        :gerbil-parser/t/fixtures/lr-construction-semantics)

(deflanguage-parser-tests prediction-products-test "Prediction products"
  (engine)
  (scenario "sparse kernel groups agree with complete closure discovery and reset scratch"
    (each (productions) (prediction-fixtures)
      (each (kernel) ('(() (0)))
        (scope ((observed (native-template-groups productions kernel)))
          (equal (car observed) (cadr observed))
          (equal (caddr observed) #t)
          (equal (cadddr observed) (car observed))))))
  (property "wide physical kernels retain distinct duplicate production addresses"
    (bindings
      (productions
        (list (list 0 'start '((nonterminal A)) 'concat #f)
              (list 1 'A (make-list 140 '(terminal word)) 'concat #f)
              (list 2 'A '() 'concat #f)
              (list 3 'A '() 'concat #f)))
      (observed (native-template-groups productions '(0 2 129 141 142 143 144))))
    (equal (car observed) (cadr observed))
    (equal (caddr observed) #t)
    (equal (cadddr observed) (car observed)))
  (property "completed dot slots do not create advanced targets"
    (bindings (observed (native-template-groups
                         '((0 start ((nonterminal A)) concat #f)
                           (1 A () concat #f)) '(1 2))))
    (equal (car observed) '())
    (equal (list-ref observed 4) '())
    (equal (list-ref observed 5) '(1 2))))
