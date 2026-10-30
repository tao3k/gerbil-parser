(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure read-source-definition))

(deflanguage-parser-tests static-object-policy-test "owned static compiler host policy"
  (engine)
  (scenario "dependencies and test objects share the same per-procedure GCC policy"
    (scope ((options (source-procedure "src/build-support/compiler-profile.ss" 'compiler-c-options '())))
      (equal (options "-O1 -fwrapv") "-O1 -fwrapv -U___SINGLE_HOST -D___MULTIPLE_HOSTS")
      (equal (options "-O2 -Iinclude") "-O2 -Iinclude -U___SINGLE_HOST -D___MULTIPLE_HOSTS")))
)
