(import (only-in :std/test test-suite test-case check check-exception)
        (only-in :std/test/base TestHarness TestConfig TestModule test-run! test-result-ok?)
        (only-in :std/io/tempfile make-temporary-file-name)
        (rename-in (only-in "./link-locks.ss" static-link-locks main) (main check-locks)))
(export main link-locks-test)

(def link-locks-test
  (test-suite "AOT lock rejection and evidence preservation"
    (test-case "dirty static closures are rejected without altering lock evidence"
  (let (directory (make-temporary-file-name "parser-aot-lock-test-"))
    (create-directory directory)
    (try
      (let (lock (path-expand "part.o.lock" directory))
        (check-locks directory)
        (call-with-output-file lock (lambda (port) (display "retained evidence" port)))
        (check (static-link-locks directory) => (list lock))
        (check-exception (check-locks directory) exception?)
        (check (call-with-input-file lock read-line) => "retained evidence")
        (delete-file lock)
        (check-locks directory))
      (finally (delete-file-or-directory directory #t)))))))

(def (main)
  (exit
    (if (test-result-ok?
          (test-run!
            (TestHarness "AOT lock control" (TestConfig verbosity: 5 capture-output?: #f)
              (list (TestModule "AOT lock control" (list link-locks-test) '() void void)))))
      0 42)))
