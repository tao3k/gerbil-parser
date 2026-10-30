;;; Prepare this owner before language owners; capture the admission boundary.
(import :gerbil-parser/language-test-support)

(def +language-execution-loaded+
  (map (lambda (name) (and (assoc name (list-modules)) #t))
       '("gerbil-parser/src/testing/language-assertions"
         "gerbil-parser/language-support/development"
         "gerbil-parser/src/compiler/lr"
         "gerbil-parser/src/runtime/lexer"
         "gerbil-parser/src/runtime/lr-parser"
         "gerbil-parser/src/runtime/incremental")))
(defparser-check (increment-check value)
  (equal (+ value 1) 42))
(deflanguage-parser-tests reusable-check "reusable fixture" (fixture)
  (scenario "fixture keeps generic lexical state"
    (scope ((value 41)) (verify increment-check value))))
(deflanguage-parser-tests test-macro-phase-test "test macro phase ownership" (engine)
  (property "generic declarations do not initialize language execution modules"
    (bindings)
    (equal +language-execution-loaded+ '(#f #f #f #f #f #f)))
  (suite "generic fixture shares the constrained assertion syntax" reusable-check)
  (matrix "matrix retains lexical bindings and axis ordering"
    (setup (base 40)) ((offset '(1 2)))
    (bindings (value (+ base offset))) (equal value (+ 40 offset))))
