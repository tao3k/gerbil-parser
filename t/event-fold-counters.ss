;;; Explicit native catalog: execution parity plus build-time source admission.
(import (only-in :std/test/base
                 TestHarness TestConfig TestModule test-run! test-result-ok?)
        (only-in :gerbil-parser/t/event-fold-test event-fold-test)
        (only-in :gerbil-parser/t/event-fold-scheme-test event-fold-scheme-test))
(export main)

(def (main . args)
  (let (result
        (test-run!
         (TestHarness "native event fold"
                      (TestConfig verbosity: 5 capture-output?: #f)
                      (list (TestModule "event fold" (list event-fold-test event-fold-scheme-test)
                                        '() void void)))))
    (unless (test-result-ok? result) (exit 1))
    (displayln "OK")
    (force-output)))
