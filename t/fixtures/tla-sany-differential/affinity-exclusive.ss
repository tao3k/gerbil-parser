(import :std/test ./worker-control)
(def test-worker-affinity 'runtime-owner)
(def test-worker-exclusive? #t)
(export test-worker-affinity test-worker-exclusive? affinity-exclusive-test)
(def affinity-exclusive-test
  (test-suite "exclusive runtime affinity"
    (test-case "resource isolation retains the same persistent runtime thread"
      (check (worker-control-runtime-owner!) => #t))))
