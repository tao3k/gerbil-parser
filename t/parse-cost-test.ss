;;; Diagnostics must preserve engine results, failure and dynamic scope.
(import :std/test
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer with-parser-cost-stage))
(export parse-cost-test)
(def parse-cost-test
  (test-suite "request-local parser cost observations"
    (test-case "disabled observation preserves zero and multiple values"
      (check (current-parser-cost-observer) => #f)
      (check (call-with-values (lambda () (with-parser-cost-stage 'zero (values))) list) => '())
      (check (call-with-values (lambda () (with-parser-cost-stage 'pair (values 1 2))) list) => '(1 2)))
    (test-case "nested observation preserves values and thrown conditions"
      (let (rows '())
        (parameterize ((current-parser-cost-observer (lambda (name row) (set! rows (cons (cons name row) rows)))))
          (check (call-with-values
                    (lambda () (with-parser-cost-stage 'outer (with-parser-cost-stage 'inner (values 3 4)))) list)
                 => '(3 4))
          (check (with-catch (lambda (e) e)
                   (lambda () (with-parser-cost-stage 'failed (raise 'original-condition)))) => 'original-condition))
        (check (current-parser-cost-observer) => #f)
        (check (map car rows) => '(failed outer inner))
        (check (cdr (assq 'completed (cdar rows))) => #f)
        (check (cdr (assq 'completed (cdadr rows))) => #t)))))
