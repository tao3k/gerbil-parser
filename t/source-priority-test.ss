(import :gerbil-parser/language-test-support)
;;; Ordered-choice lowering stays pure and preserves empty matched branches.
(import :std/test
        (only-in "../src/language/source-pattern-funs.ss"
                 source-priority-forms))

(deflanguage-parser-tests source-priority-test "source priority lowering"
  (engine)
    (property "priority and empty branches retain exact event-fold structure"
      (bindings)
      (equal (source-priority-forms '((bool #t) (bool #f))
                                   '(() ((token Match start end)))
                                   '((token Fallback start end)))
         '((if (bool #t) ()
                 ((if (bool #f) ((token Match start end))
                      ((token Fallback start end)))))))
      (equal (source-priority-forms '() '() '((token Fallback start end)))
         '((token Fallback start end)))
      (equal (source-priority-forms '((bool #t)) '(()))  '((if (bool #t) () ()))))
    (scenario "invalid branch shapes are rejected before lowering"
      (each (args) ('(((a) ()) ((a) (b)) ((a . b) (())) (() () #f)))
          (raises (apply source-priority-forms args) true))))
