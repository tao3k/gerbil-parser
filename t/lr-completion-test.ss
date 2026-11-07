(import :gerbil-parser/language-test-support)
;;; Completion policy invariants independent of parser exploration order.
(import :std/test
        (only-in :gerbil-parser/src/runtime/lr-completion
                 make-candidate select-candidate candidate-root candidate-rest
                 candidate-score candidate-ambiguities candidate-winner-reason
                 candidate-completion-count))


(def (completion-summary candidate)
  (list (candidate-root candidate) (candidate-rest candidate)
        (candidate-score candidate) (candidate-ambiguities candidate)
        (candidate-winner-reason candidate) (candidate-completion-count candidate)))

(defparser-check (check-selection left right expected merged?)
  (scope-values (((winner merged) (select-candidate left right)))
    (equal (completion-summary winner)  expected)
    (equal merged  merged?)))

(deflanguage-parser-tests lr-completion-test "selective GLR completion policy"
  (engine)
    (scenario "first completion preserves identity"
      (scope ((candidate (make-candidate 'Root '(tail) 0)))
        (scope-values (((winner merged?) (select-candidate #f candidate)))
          (equal (eq? winner candidate)  #t)
          (equal merged?  #f))))
    (scenario "dynamic score outranks consumption in either exploration order"
      (scope ((low (make-candidate 'Low '() 1))
            (high (make-candidate 'High '(a b) 2)))
        (verify check-selection low high '(High (a b) 2 0 dynamic-precedence 2) #f)
        (verify check-selection high low '(High (a b) 2 0 dynamic-precedence 2) #f)
        (equal (completion-summary high)  '(High (a b) 2 0 unique-completion 1))))
    (scenario "maximal consumption ranks equal-score suffixes"
      (scope ((partial (make-candidate 'Partial '(a b) 0))
            (complete (make-candidate 'Complete '() 0)))
        (verify check-selection partial complete '(Complete () 0 0 maximal-consumption 2) #f)
        (verify check-selection complete partial '(Complete () 0 0 maximal-consumption 2) #f)))
    (scenario "equivalent roots and suffixes merge structural copies"
      (verify check-selection (make-candidate (list 'Root) (list 'tail) 0) (make-candidate (list 'Root) (list 'tail) 0) '((Root) (tail) 0 0 equivalent-merge 2) #t)
      (verify check-selection (make-candidate 'Root '(tail) 0 2 'ambiguous 3) (make-candidate 'Root '(tail) 0 1 'ambiguous 4) '(Root (tail) 0 3 ambiguous 7) #t))
    (scenario "distinct products retain ambiguity until an outer score wins"
      (scope-values (((ambiguous merged?)
                    (select-candidate (make-candidate 'First '(a) 0)
                                      (make-candidate 'Second '(a) 0))))
        (equal (completion-summary ambiguous)  '(First (a) 0 1 ambiguous 2))
        (equal merged?  #f)
        (verify check-selection ambiguous (make-candidate 'Ranked '(a b) 1) '(Ranked (a b) 1 0 dynamic-precedence 3) #f))
      (verify check-selection (make-candidate 'Root '(a) 0) (make-candidate 'Root '(b) 0) '(Root (a) 0 1 ambiguous 2) #f)))
