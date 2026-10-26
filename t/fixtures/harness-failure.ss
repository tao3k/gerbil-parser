;;; Deliberately failing Suite, selected only by the development/CI entry.
(import :std/test)
(export harness-failure-test)
(def harness-failure-test
  (test-suite "native harness rejection control"
    (test-case "intentional failure reaches the invoking qualification entry"
      (check #f => #t))))
