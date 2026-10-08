(import :std/test ./worker-control)
(export worker-right-test)
(worker-control-loaded!)
(def worker-right-test
  (test-suite "worker right"
    (test-case "all modules load before shared-runtime Workers rendezvous"
      (check (worker-control-loaded-count) => 2)
      (worker-control-rendezvous!)
      (check (worker-control-loaded-count) => 2))))
