;;; Suite declarations only; the development command invokes gerbil test.
(import :std/test)
(include "../test-plan.ss")
(export test-plan-test)
(def test-plan-test
  (test-suite "native test build selection"
    (test-case "test modules remain source Suite inputs"
      (check (conformance-source-only? "t/a-test") => #t)
      (check (and (conformance-source-only? "t/research-main") #t) => #t)
      (check (conformance-source-only? "t/helper") => #f))
    (test-case "declared conformance Suites exist without a private executor"
      (let (files (conformance-covered-test-files))
        (check (> (length files) 0) => #t)
        (for-each (lambda (file) (check (file-exists? file) => #t)) files)))))
