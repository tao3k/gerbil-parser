;;; Ordered-choice lowering stays pure and preserves empty matched branches.
(import :std/test
        (only-in "../src/modules/parser/source-pattern-funs.ss"
                 source-priority-forms))
(export source-priority-test)
(def source-priority-test
  (test-suite "source priority lowering"
    (test-case "priority and empty branches retain exact event-fold structure"
      (check (source-priority-forms '((bool #t) (bool #f))
                                   '(() ((token Match start end)))
                                   '((token Fallback start end)))
        => '((if (bool #t) ()
                 ((if (bool #f) ((token Match start end))
                      ((token Fallback start end)))))))
      (check (source-priority-forms '() '() '((token Fallback start end)))
        => '((token Fallback start end)))
      (check (source-priority-forms '((bool #t)) '(())) => '((if (bool #t) () ()))))
    (test-case "invalid branch shapes are rejected before lowering"
      (for-each
        (lambda (args)
          (check-exception (apply source-priority-forms args) true))
        '(((a) ()) ((a) (b)) ((a . b) (())) (() () #f))))))
