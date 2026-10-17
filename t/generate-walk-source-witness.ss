;;; Serialize the current reader AST; check mode verifies a proof input is fresh.
(import (only-in :gerbil-parser/t/fixtures/event-walk-source
                 read-event-walk-source write-event-walk-source-witness event-walk-witness-current?))
(export main)
(def (main mode output . sources)
  (unless (member mode '("module" "check")) (error "expected module|check output [source]"))
  (let* ((source (read-event-walk-source
                 (if (null? sources) "src/runtime/event-program.ss" (car sources))))
         (emit (lambda (port) (write-event-walk-source-witness source port))))
    (if (equal? mode "module") (call-with-output-file output emit)
      (unless (event-walk-witness-current? source output)
        (error "Scheme source proof input is stale" output)))))
