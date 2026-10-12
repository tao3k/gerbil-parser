(import :std/test (only-in ../src/compiler/event-fold-analysis fold-eliminate-inert-branches))
(export fold-inert-test)

(def fold-inert-test
  (test-suite "Effect-admitted empty Fold branches"
    (test-case "nested pure state observations disappear"
      (check (fold-eliminate-inert-branches
              '((if (uint-positive? (state mode))
                    ((if (offset-less? start (state-offset cursor)) () ())) ())))
             => '()))
    (test-case "range errors queries and unknown effects remain"
      (for-each
       (lambda (predicate)
         (let (forms (list (list 'if predicate '() '())))
           (check (fold-eliminate-inert-branches forms) => forms)))
       '((line-bytes-any-in? start end (32))
         (future-heading-title? "x") (unknown-predicate))))
    (test-case "writes events and frame validation retain order"
      (check (fold-eliminate-inert-branches
              '((if (bool #t) ((set-uint mode (uint 1))) ())
                (with-source-bounds start end ((if (bool #f) () ())))
                (token Text start end)))
             => '((if (bool #t) ((set-uint mode (uint 1))) ())
                  (with-source-bounds start end ())
                  (token Text start end))))
    (test-case "only validated ordered cursor ranges admit discarded scans"
      (let* ((forward '(if (line-bytes-any-in? (line-index byte)
                                              (line-step (line-index byte)) (32)) () ()))
             (backward '(if (line-bytes-any-in? (line-step (line-index byte))
                                               (line-index byte) (32)) () ()))
             (frame `(with-source-bounds start end (,forward))))
        (check (fold-eliminate-inert-branches (list forward) 'byte) => '())
        (check (fold-eliminate-inert-branches (list forward)) => (list forward))
        (check (fold-eliminate-inert-branches (list backward) 'byte) => (list backward))
        (check (fold-eliminate-inert-branches (list frame) 'byte) => (list frame))))))
