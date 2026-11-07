;;; Physical LR0 publication and reverse suffix producer controls.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/lr-state-semantics
                 native-closure-items native-suffix-slots))

(deflanguage-parser-tests lr-state-semantics-test "LR state and suffix semantics"
  (engine)
  (scenario "source merge retains increasing order, overlap and arbitrary-width core IDs"
    (each (row)
      ('((() () ())
         (() (0 65 130) (0 65 130))
         ((0 65 130) () (0 65 130))
         ((0) (0) (0))
         ((0 65 2048) (1 65 130) (0 1 65 130 2048))
         ((1 3) (0 2 4) (0 1 2 3 4))))
      (equal (native-closure-items (car row) (cadr row)) (caddr row))))
  (property "all physical suffix slots retain unproductive-prefix FIRST and completed nullability"
    (bindings (observed (native-suffix-slots)))
    (equal (car observed)
      '((12 #f) (2 #f) (0 #t)
        (12 #f) (4 #f) (0 #t)
        (8 #f) (0 #f) (0 #f) (0 #t)
        (0 #t) (12 #f) (0 #t) (0 #f) (0 #t)))
    (equal (cadr observed) 1)))
