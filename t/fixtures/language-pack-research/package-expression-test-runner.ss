#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Research infrastructure: run the declarative suite and propagate its result.
(import "package-expression-parser-test"
        (only-in :std/test/base TestConfig current-test-config VERBOSITY-CASE
                 test-suite! test-result-ok?))
(def result
  (parameterize ((current-test-config (TestConfig verbosity: VERBOSITY-CASE capture-output?: #f)))
    (test-suite! package-expression-parser-test)))
(unless (test-result-ok? result)
  (display "PACKAGE-EXPRESSION-DSL-FAILED") (newline) (force-output)
  (exit 1))
(display "PACKAGE-EXPRESSION-DSL-OK") (newline) (force-output)
(exit 0)
