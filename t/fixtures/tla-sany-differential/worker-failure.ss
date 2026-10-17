(import :std/test)
(export worker-failure-test)
(def worker-failure-test
  (test-suite "failure propagation"
    (test-case "intentional failing assertion" (check #f => #t))))
