;;; Link the unchanged native gxtest suite; do not load compiler modules at startup.
(import (only-in :std/test/base
                 TestHarness TestConfig TestModule test-run! test-result-ok?)
        (only-in :gerbil-parser/t/event-fold-test event-fold-test))
(export main)

(def (main . args)
  (let (result
        (test-run!
         (TestHarness "native event fold"
                      (TestConfig verbosity: 5 capture-output?: #f)
                      (list (TestModule "event fold" (list event-fold-test)
                                        '() void void)))))
    (unless (test-result-ok? result) (exit 1))
    (displayln "OK")
    (force-output)))
