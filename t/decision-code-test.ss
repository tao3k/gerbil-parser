(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/compiler/decision-code shared-decision-expression))

;;; Independent unshared source oracle; tests execute both forms in the same
;;; lexical environment, including effectful predicates and action results.
(def (unshared-decision selector rows fallback)
  `(case ,selector
     ,@(map (lambda (row) `((,(car row)) (cond ,@(cdr row) (else ,fallback)))) rows)
     (else ,fallback)))

(def (run-decision form state input)
  ((eval `(lambda (state input)
            (let (observed '())
              (def (probe key)
                (set! observed (cons key observed))
                (member key input))
              (let (answer ,form) (list answer (reverse observed)))))) state input))

(deflanguage-parser-tests decision-code-test "shared decision code"
  (engine)
  (scenario "ordered predicates and handler effects agree with unshared source"
    (scope ((rows '((0 ((probe 'a) (list 'first input)) ((probe 'b) (list 'second input)))
                   (1 ((probe 'b) (list 'second input)) ((probe 'a) (list 'first input)))
                   (2 ((probe 'a) #f) ((probe 'b) (list 'second input)))))
            (fallback '(list 'fallback input)))
      (each (state) ('(-1 0 1 2 3))
        (each (input) ('(() (a) (b) (a b) (b a) (other)))
          (equal (run-decision (shared-decision-expression 'state rows fallback) state input)
                 (run-decision (unshared-decision 'state rows fallback) state input))))))
  (scenario "zero action code and false results never select fallback"
    (scope ((rows '((0 (#t #f)) (1 (#t #f)))))
      (equal (run-decision (shared-decision-expression 'state rows ''fallback) 0 '()) '(#f ()))
      (equal (run-decision (shared-decision-expression 'state rows ''fallback) 1 '()) '(#f ()))))
  (scenario "empty rows and unknown states preserve fallback without predicates"
    (equal (run-decision (shared-decision-expression 'state '() ''fallback) 0 '()) '(fallback ())))
  (scenario "duplicate bodies have one emitted handler and deterministic codes"
    (scope ((rows '((0 ((probe 'a) (list 'same input)) ((probe 'b) (list 'same input)))
                   (1 ((probe 'c) (list 'other input)) ((probe 'a) (list 'same input))))))
      (scope ((form (shared-decision-expression 'state rows ''fallback)))
        (equal form (shared-decision-expression 'state rows ''fallback))
        (equal (length (cddr form)) 3)))))
