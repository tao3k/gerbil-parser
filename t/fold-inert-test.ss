(import :gerbil-parser/language-test-support)
(import :std/test (only-in ../src/compiler/event-fold-analysis fold-eliminate-inert-branches))


(deflanguage-parser-tests fold-inert-test "Effect-admitted empty Fold branches"
  (engine)
    (property "nested pure state observations disappear"
      (bindings)
      (equal (fold-eliminate-inert-branches
              '((if (uint-positive? (state mode))
                    ((if (offset-less? start (state-offset cursor)) () ())) ())))
              '()))
    (scenario "range errors queries and unknown effects remain"
      (each (predicate) ('((line-bytes-any-in? start end (32))
         (future-heading-title? "x") (unknown-predicate)))
         (scope ((forms (list (list 'if predicate '() '()))))
           (equal (fold-eliminate-inert-branches forms)  forms))))
    (property "writes events and frame validation retain order"
      (bindings)
      (equal (fold-eliminate-inert-branches
              '((if (bool #t) ((set-uint mode (uint 1))) ())
                (with-source-bounds start end ((if (bool #f) () ())))
                (token Text start end)))
              '((if (bool #t) ((set-uint mode (uint 1))) ())
                  (with-source-bounds start end ())
                  (token Text start end))))
    (property "only validated ordered cursor ranges admit discarded scans"
      (bindings (forward '(if (line-bytes-any-in? (line-index byte)
                                              (line-step (line-index byte)) (32)) () ())) (backward '(if (line-bytes-any-in? (line-step (line-index byte))
                                               (line-index byte) (32)) () ())) (frame `(with-source-bounds start end (,forward))))
      (equal (fold-eliminate-inert-branches (list forward) 'byte)  '())
      (equal (fold-eliminate-inert-branches (list forward))  (list forward))
      (equal (fold-eliminate-inert-branches (list backward) 'byte)  (list backward))
      (equal (fold-eliminate-inert-branches (list frame) 'byte)  (list frame))))
