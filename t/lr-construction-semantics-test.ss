;;; Whole-producer controls complement the universal construction proofs.
(import :gerbil-parser/language-test-support
        :gerbil-parser/t/fixtures/lr-construction-semantics
        (only-in "fixtures/construction-lifecycle.ss" construction-observation))

(deflanguage-parser-tests lr-construction-semantics-test "LR construction protocols"
  (engine)
  (property "native prediction queues converge through cycles and retain duplicate rule IDs"
    (bindings (observed (native-predictions
         '((0 start ((nonterminal A)) concat #f)
           (1 A ((nonterminal B)) concat #f)
           (2 B ((nonterminal A)) concat #f)
           (3 B () concat #f)
           (4 B () concat #f)))))
    (equal (car observed) '(30 0 30 0 30 0 0 0))
      (equal (cadr observed) '(1 0 1)))
  (scenario "native state storage grows while preserving every published row"
    (each (width) ('(0 1 127 128 160))
      (equal (native-state-chain width)
        (list (+ width 1) (+ width 1) (if (< width 128) 128 256) #t #t))))
  (property "recursive target kernels are interned once"
    (bindings (observed (native-recursive-states)))
    (equal (car observed) 4)
    (equal (cadr observed) 4)
    (equal (caddr observed) '((0 2 5) (1) (2 3 5) (4)))
    (equal (cadddr observed)
      '((((nonterminal A) . 1) ((terminal token a) . 2)) ()
        (((terminal token a) . 2) ((nonterminal A) . 3)) ())))
  (scenario "complete queue publishes every ID once while borrowed scratch resets"
    (each (productions) (prediction-fixtures)
      (scope ((observed (construction-observation productions)))
        (equal (list-ref observed 1) (car observed))
        (equal (list-ref observed 2) (iota (car observed)))
        (equal (list-ref observed 3) (iota (car observed)))
        (equal (list-ref observed 4) (iota (car observed)))
        (equal (list-ref observed 5) (car observed))
        (equal (list-ref observed 6) #t))))
  (property "complete publication retains FIFO IDs across physical vector growth"
    (bindings
      (productions (list (list 0 'start
        (map (lambda (id) (list 'terminal 'token id)) (iota 129)) 'concat #f)))
      (observed (construction-observation productions)))
    (equal (car observed) 130)
    (equal (list-ref observed 1) 130)
    (equal (list-ref observed 2) (iota 130))
    (equal (list-ref observed 3) (iota 130))
    (equal (list-ref observed 4) (iota 130))
    (equal (list-ref observed 6) #t))
  (property "two-pass coordinates retain empty states and repeated core IDs"
    (bindings (observed (native-coordinates '(() (0 65 2048) () (0 130))
                          '((0 0) (1 0) (1 65) (1 2048) (1 66) (2 0) (3 0) (3 130)))))
    (equal observed '((0 0 3 3 5) (0 65 2048 0 130) 5 (#f 0 1 2 #f #f 3 4)))))
