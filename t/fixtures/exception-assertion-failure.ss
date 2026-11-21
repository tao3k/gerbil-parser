(import :gerbil-parser/language-support/testing)

(deflanguage-parser-tests exception-assertion-failure-test "Expected exception assertion failure"
  (engine)
    (scenario "a normal value cannot be mistaken for an exception"
      (raises 42 true)))
