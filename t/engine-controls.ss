;;; -*- Gerbil -*-
;;; Compile source-owned suites into one std/test entry; no interpreter
;;; module expansion or child-process scheduler belongs to these controls.
(import (only-in :std/test/base TestHarness TestModule TestConfig test-run! test-result-ok?)
        (only-in ./event-fold-scheme-test event-fold-scheme-test)
        (only-in ./ranked-regular-scanner-test ranked-regular-scanner-test)
        (only-in ./parser-runtime-test parser-runtime-test))
(export main)

(def (main . args)
  (let* ((modules
          (map (lambda (entry)
                 (TestModule (car entry) (list (cdr entry)) '() void void))
               (list (cons "fold-source" event-fold-scheme-test)
                     (cons "ranked-regular-scanner" ranked-regular-scanner-test)
                     (cons "parser-runtime" parser-runtime-test))))
         (result (test-run! (TestHarness "Engine controls"
                              (TestConfig verbosity: 5 capture-output?: #f) modules))))
    (if (test-result-ok? result)
      (begin (displayln "OK") (force-output))
      (begin (displayln "FAILED engine controls") (force-output) (exit 42)))))
