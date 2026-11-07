(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/support/qualification/build qualification-build-spec))

(deflanguage-parser-tests static-object-policy-test "qualification build ownership"
  (engine)
  (scenario "the native owner declares the executable in the standard build format"
    (equal (take (car (qualification-build-spec)) 4)
           '(exe: "t/support/qualification/process-child"
                  bin: "gerbil-parser-process-child"))))
