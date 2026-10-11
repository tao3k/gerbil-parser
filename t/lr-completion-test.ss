;;; Completion policy invariants independent of parser exploration order.
(import :std/test
        (only-in :gerbil-parser/src/runtime/lr-completion
                 make-candidate select-candidate candidate-root candidate-rest
                 candidate-score candidate-ambiguities candidate-winner-reason
                 candidate-completion-count))
(export lr-completion-test)

(def (completion-summary candidate)
  (list (candidate-root candidate) (candidate-rest candidate)
        (candidate-score candidate) (candidate-ambiguities candidate)
        (candidate-winner-reason candidate) (candidate-completion-count candidate)))

(def (check-selection left right expected merged?)
  (let-values (((winner merged) (select-candidate left right)))
    (check (completion-summary winner) => expected)
    (check merged => merged?)))

(def lr-completion-test
  (test-suite "selective GLR completion policy"
    (test-case "first completion preserves identity"
      (let (candidate (make-candidate 'Root '(tail) 0))
        (let-values (((winner merged?) (select-candidate #f candidate)))
          (check (eq? winner candidate) => #t)
          (check merged? => #f))))
    (test-case "dynamic score outranks consumption in either exploration order"
      (let ((low (make-candidate 'Low '() 1))
            (high (make-candidate 'High '(a b) 2)))
        (check-selection low high '(High (a b) 2 0 dynamic-precedence 2) #f)
        (check-selection high low '(High (a b) 2 0 dynamic-precedence 2) #f)
        (check (completion-summary high) => '(High (a b) 2 0 unique-completion 1))))
    (test-case "maximal consumption ranks equal-score suffixes"
      (let ((partial (make-candidate 'Partial '(a b) 0))
            (complete (make-candidate 'Complete '() 0)))
        (check-selection partial complete '(Complete () 0 0 maximal-consumption 2) #f)
        (check-selection complete partial '(Complete () 0 0 maximal-consumption 2) #f)))
    (test-case "equivalent roots and suffixes merge structural copies"
      (check-selection (make-candidate (list 'Root) (list 'tail) 0)
                       (make-candidate (list 'Root) (list 'tail) 0)
                       '((Root) (tail) 0 0 equivalent-merge 2) #t)
      (check-selection (make-candidate 'Root '(tail) 0 2 'ambiguous 3)
                       (make-candidate 'Root '(tail) 0 1 'ambiguous 4)
                       '(Root (tail) 0 3 ambiguous 7) #t))
    (test-case "distinct products retain ambiguity until an outer score wins"
      (let-values (((ambiguous merged?)
                    (select-candidate (make-candidate 'First '(a) 0)
                                      (make-candidate 'Second '(a) 0))))
        (check (completion-summary ambiguous) => '(First (a) 0 1 ambiguous 2))
        (check merged? => #f)
        (check-selection ambiguous (make-candidate 'Ranked '(a b) 1)
                         '(Ranked (a b) 1 0 dynamic-precedence 3) #f))
      (check-selection (make-candidate 'Root '(a) 0)
                       (make-candidate 'Root '(b) 0)
                       '(Root (a) 0 1 ambiguous 2) #f))))
