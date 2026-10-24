(import :std/test)
(export worker-timeout-test)
(def worker-timeout-test
  (test-suite "silence watchdog"
    (test-case "intentional real silence" (thread-sleep! 6))))
