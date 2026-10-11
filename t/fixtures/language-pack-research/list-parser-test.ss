;;; -*- Gerbil -*-
;;; Independent conformance declarations for the canonical composed package.
(import :gerbil-parser/language-test-support "list-parser")

(deflanguage-parser-tests list-parser-test "canonical POO composition package DSL"
  (loader list-composed-language)
  (identity "package identity"
    (language "list-study") (version "v1") (contract "list-study.local.v1")
    (schema "gerbil-parser.language-entry.v2"))
  (accepted "empty call" "f()"
    (root SourceFile) (counts (Call 1) (Name 0))
    (field-counts Call argument (0)))
  (accepted "empty array" "[]"
    (root SourceFile) (counts (Array 1) (Name 0))
    (field-counts Array element (0)))
  (accepted "single call argument" "f(a)"
    (counts (Call 1) (Name 1)) (field-counts Call argument (1)))
  (accepted "three call arguments" "f(a,b,c)"
    (counts (Call 1) (Name 3)) (field-counts Call argument (3)))
  (accepted "call whitespace" "f( a , b )"
    (counts (Call 1) (Name 2)) (field-counts Call argument (2)))
  (accepted "single array element" "[a]"
    (counts (Array 1) (Name 1)) (field-counts Array element (1)))
  (accepted "three array elements" "[a,b,c]"
    (counts (Array 1) (Name 3)) (field-counts Array element (3)))
  (accepted "array whitespace and newline" "[ a ,\n b ]"
    (counts (Array 1) (Name 2)) (field-counts Array element (2)))
  (rejected-many "separator and closing policies"
    '("f(,)" "f(a,)" "f(a b)" "f(a" "[,]" "[a,]" "[a b]" "[a"))
  (ast "exact empty call tree and spans" "f()"
    (node SourceFile 0 3
      (field form 0 3
        (node Call 0 3
          (field callee 0 1 (token identifier "f" 0 1))
          (token punctuation "(" 1 2)
          (token punctuation ")" 2 3))))))
