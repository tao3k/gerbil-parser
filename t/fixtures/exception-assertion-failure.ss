(import :gerbil-parser/language-test-support)

(deflanguage-parser-tests exception-assertion-failure-test "Expected exception assertion failure"
  (engine)
    (scenario "a normal value cannot be mistaken for an exception"
      (raises 42 true)))
