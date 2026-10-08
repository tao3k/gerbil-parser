(import :std/test ./worker-control)
(export worker-left-test)
(worker-control-loaded!)
(def worker-left-test
  (test-suite "worker left"
    (test-case "all modules load before shared-runtime Workers rendezvous"
      (check (worker-control-loaded-count) => 2)
      (worker-control-rendezvous!)
      (check (worker-control-loaded-count) => 2))))
