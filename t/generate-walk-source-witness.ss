;;; Serialize the current reader AST; check mode verifies a proof input is fresh.
(import (only-in "fixtures/event-walk-source.ss"
                 read-event-walk-source write-event-walk-source-data event-walk-data-current?))
(export main)
(def (main mode output . sources)
  (unless (member mode '("module" "check")) (error "expected module|check output [source]"))
  (let* ((source (read-event-walk-source
                 (if (null? sources) "src/runtime/event-program.ss" (car sources))))
         (emit (lambda (port) (write-event-walk-source-data source port))))
    (if (equal? mode "module")
      ;; Finish serialization before opening the destination: unsupported reader
      ;; data must leave the previous proof input intact.
      (let (rendered (call-with-output-string emit))
        (call-with-output-file output (lambda (port) (write-string rendered port))))
      (unless (event-walk-data-current? source output)
        (error "Scheme source proof input is stale" output)))))
