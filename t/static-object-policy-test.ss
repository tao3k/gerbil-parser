(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .get .call)
        (only-in "../init.ss" gerbil-parser.init.exports)
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure read-source-definition))

(deflanguage-parser-tests static-object-policy-test "owned static compiler host policy"
  (engine)
  (scenario "dependencies and test objects share the same per-procedure GCC policy"
    (scope ((options (source-procedure "src/build-support/compiler-profile.ss" 'compiler-c-options '())))
      (equal (options "-O1 -fwrapv") "-O1 -fwrapv -U___SINGLE_HOST -D___MULTIPLE_HOSTS")
      (equal (options "-O2 -Iinclude") "-O2 -Iinclude -U___SINGLE_HOST -D___MULTIPLE_HOSTS")))
  (scenario "Core derives support dependencies without a hand-ordered catalog"
    (sequential ((demand (.call (.get gerbil-parser.init.exports tests) .for-files
                                (list (path-expand "t/prediction-products-test.ss"))))
                 (spec ((.get demand std-make-spec)))
                 (sources (map (lambda (entry)
                                 (path-normalize (path-expand (path-strip-extension (cadr entry))))) spec)))
      (each (source) ('("t/fixtures/prediction-witness" "t/fixtures/event-walk-source"
                        "t/fixtures/lr-construction-semantics"))
        (equal (and (member (path-normalize (path-expand source)) sources) #t) #t))
      (equal (and (member (path-normalize (path-expand "t/prediction-products-test")) sources) #t) #f)))
  (scenario "research closure rejects Suite dependencies even if artifacts exist"
    (scope ((admit (source-procedure "build-language-conformance-link.ss" 'validate-research-product-closure! '())))
      (equal (admit '("gerbil-parser/languages/tla-plus/fixtures" "std/test/base"))
             '("gerbil-parser/languages/tla-plus/fixtures" "std/test/base"))
      (raises (admit '("gerbil-parser/languages/tla-plus/parser-test")) exception?)))
)
