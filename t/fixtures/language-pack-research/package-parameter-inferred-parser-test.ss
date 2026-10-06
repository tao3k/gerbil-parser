;;; -*- Gerbil -*-
(import :gerbil-parser/language-test-support "package-parameter-inferred-parser")
(deflanguage-parser-tests package-parameter-inferred-test "parameter fields inferred package DSL"
  (loader list-parameter-inferred-language)
  (syntax-kind "exact effective Call field catalog" Call node (callee parameter))
  (accepted "empty call emits no argument or parameter" "f()"
    (field-counts Call argument (0)) (field-counts Call parameter (0)))
  (accepted "three parameters without historical argument fields" "f(a,b,c)"
    (counts (Call 1) (Name 3))
    (field-counts Call parameter (3)) (field-counts Call argument (0)))
  (rejected-many "separator policies unchanged"
    '("f(,)" "f(a,)" "f(a b)" "f(a"))
  (ast "exact single parameter tree without fabricated optional fields" "f(a)"
    (node SourceFile 0 4
      (field form 0 4
        (node Call 0 4
          (field callee 0 1 (token identifier "f" 0 1))
          (token punctuation "(" 1 2)
          (field parameter 2 3
            (node Name 2 3
              (field value 2 3 (token identifier "a" 2 3))))
          (token punctuation ")" 3 4))))))
