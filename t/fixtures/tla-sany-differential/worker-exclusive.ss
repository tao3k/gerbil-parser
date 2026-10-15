(import :std/test ./worker-control)
(def test-worker-affinity 'exclusive)
(export test-worker-affinity worker-exclusive-test)
(def worker-exclusive-test
  (test-suite "exclusive execution admission"
    (test-case "exclusive owner waits for every parallel owner to finish"
      (check (worker-control-loaded-count) => 2)
      (check (worker-control-finished?) => #t))))
