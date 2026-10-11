#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Research infrastructure: run the declarative suite and propagate its result.
(import "package-parameter-retained-parser-test"
        (only-in :std/test/base TestConfig current-test-config VERBOSITY-CASE
                 test-suite! test-result-ok?))
(def result
  (parameterize ((current-test-config (TestConfig verbosity: VERBOSITY-CASE capture-output?: #f)))
    (test-suite! package-parameter-retained-test)))
(unless (test-result-ok? result)
  (display "PACKAGE-PARAMETER-RETAINED-DSL-FAILED") (newline) (force-output)
  (exit 1))
(display "PACKAGE-PARAMETER-RETAINED-DSL-OK") (newline) (force-output)
(exit 0)
