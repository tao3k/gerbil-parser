(import :gerbil-parser/language-test-support)
(import (only-in :std/io/tempfile make-temporary-file-name)
        (rename-in (only-in "./link-locks.ss" static-link-locks main) (main check-locks)))

(deflanguage-parser-tests link-locks-test "AOT lock rejection and evidence preservation"
  (engine)
    (scenario "dirty static closures are rejected without altering lock evidence"
  (scope ((directory (make-temporary-file-name "parser-aot-lock-test-")))
    (act (create-directory directory))
    (protect (body
      (scope ((lock (path-expand "part.o.lock" directory)))
        (act (check-locks directory))
        (act (call-with-output-file lock (lambda (port) (display "retained evidence" port))))
        (equal (static-link-locks directory)  (list lock))
        (raises (check-locks directory) exception?)
        (equal (call-with-input-file lock read-line)  "retained evidence")
        (act (delete-file lock))
        (act (check-locks directory)))
      ) (cleanup (act (delete-file-or-directory directory #t)))))))
