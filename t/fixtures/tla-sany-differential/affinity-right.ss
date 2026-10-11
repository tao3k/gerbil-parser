(import :std/test ./worker-control)
(def test-worker-affinity 'runtime-owner)
(export test-worker-affinity affinity-test)
(def affinity-test
  (test-suite "persistent runtime owner"
    (test-case "declared modules execute on the same Worker thread"
      (check (worker-control-runtime-owner!) => #t))))
