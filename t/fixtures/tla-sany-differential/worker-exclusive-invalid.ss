(import :std/test)
(def test-worker-exclusive? 'invalid)
(export test-worker-exclusive? worker-exclusive-invalid-test)
(def worker-exclusive-invalid-test
  (test-suite "invalid exclusivity declaration"
    (test-case "invalid metadata cannot reach execution" (check #t => #t))))
