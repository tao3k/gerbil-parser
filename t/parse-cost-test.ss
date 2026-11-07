(import :gerbil-parser/language-test-support)
;;; Diagnostics must preserve engine results, failure and dynamic scope.
(import :std/test
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer with-parser-cost-stage admit-parser-allocation))

(deflanguage-parser-tests parse-cost-test "request-local parser cost observations"
  (engine)
    (property "allocation admission preserves signed counter and GC boundaries"
      (bindings)
      (equal (admit-parser-allocation 128. 0.)  128.)
      (equal (admit-parser-allocation 0. 0.)  0.)
      (equal (admit-parser-allocation -64. 0.)  #f)
      (equal (admit-parser-allocation 128. 1.)  #f)
      (equal (admit-parser-allocation -64. 1.)  #f))
    (property "disabled observation preserves zero and multiple values"
      (bindings)
      (equal (current-parser-cost-observer)  #f)
      (equal (call-with-values (lambda () (with-parser-cost-stage 'zero (values))) list)  '())
      (equal (call-with-values (lambda () (with-parser-cost-stage 'pair (values 1 2))) list)  '(1 2)))
    (scenario "nested observation preserves values and thrown conditions"
      (scope ((rows '()))
        (parameters ((current-parser-cost-observer (lambda (name row) (set! rows (cons (cons name row) rows)))))
          (equal (call-with-values
                    (lambda () (with-parser-cost-stage 'outer (with-parser-cost-stage 'inner (values 3 4)))) list)
                  '(3 4))
          (equal (with-catch (lambda (e) e)
                   (lambda () (with-parser-cost-stage 'failed (raise 'original-condition))))  'original-condition))
        (equal (current-parser-cost-observer)  #f)
        (equal (map car rows)  '(failed outer inner))
        (equal (cdr (assq 'completed (cdar rows)))  #f)
        (equal (cdr (assq 'completed (cdadr rows)))  #t))))
