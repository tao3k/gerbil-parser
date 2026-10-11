(import :std/test)
(def alias-left-test
  (test-suite "alias control" (test-case "should never execute" (error "duplicate Suite executed"))))
(def alias-right-test alias-left-test)
(export alias-left-test alias-right-test)
