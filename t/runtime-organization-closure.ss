;;; -*- Gerbil -*-
;;; Existing gxtest values linked ahead of time; no request-time expansion.
(import (only-in :std/test/base TestHarness TestConfig TestModule test-run! test-result-ok?)
        (only-in :std/test test-suite test-case check)
        "runtime-module-organization-test.ss"
        (only-in "test-style-contract-test.ss" test-style-contract-tests)
        (only-in "benchmarks/parser-throughput/link-locks-test.ss" link-locks-test)
        "lr-semantic-preparation-test.ss"
        "lr-stack-reduction-test.ss"
        "lr-action-preparation-test.ss"
        "lr-fragment-transfer-test.ss"
        "recovery-incremental-test.ss")
(export main)
(def (main . arguments)
  (unless (or (null? arguments) (equal? arguments '("--failure-control")))
    (error "runtime organization closure accepts no suite filters"))
  (let* ((suites (if (null? arguments)
                  (list runtime-module-organization-test test-style-contract-tests link-locks-test lr-semantic-preparation-test
                        lr-stack-reduction-test lr-action-preparation-test lr-ordinary-action-preparation-test
                        lr-fragment-transfer-test recovery-incremental-test)
                  (list (test-suite "failure exit control"
                          (test-case "intentional failure reaches the process exit"
                            (check #f => #t))))))
         (result (test-run!
                   (TestHarness "native runtime organization"
                                (TestConfig verbosity: 5 capture-output?: #f)
                                (list (TestModule "native runtime organization" suites '() void void))))))
    (if (test-result-ok? result)
      (begin (displayln "OK") (force-output) (exit 0))
      (begin (displayln "FAILED") (force-output) (exit 42)))))
