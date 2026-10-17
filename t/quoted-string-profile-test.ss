(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Strict quoted text is shared lexical data, independent of language callbacks.
(import :std/test
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/src/grammar/lexical-algebra lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-quoted-string/profile)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip))
(export  quoted-profile-language-grammar)

(begin
 (deflanguage quoted-profile
  (syntax
   (lexical
    (root document)
    (lex (string String (quoted-string-profile "'" "`'\\/fnrt" 4))
       (name Name (quoted-string-profile "`" "`'\\/fnrt" 4))
       (unknown Unknown (fallback)))))
  (rules (document (node SourceFile (field item (choice string name))))))
 (bind-fixture-grammar-release quoted-profile "quoted-profile" "v1" "quoted-profile.v1") )

(deflanguage-parser-tests quoted-string-profile-test "shared quoted string profiles"
  (engine)
    (scenario "strict escape profiles retain valid and invalid endpoints"
      (each (row) ('(("'α'next" 3) ("'a''b'" 3) ("'a\nb'" 5) ("'\\n'" 4)
         ("'\\u0041'" 8) ("'\\u١٢٣٤'" 8) ("'\\u00411'" 9)
         ("'\\x'" #f) ("'\\u123'" #f) ("'\\uGGGG'" #f) ("'\\u²Ⅻ½４'" #f) ("'\\" #f) ("'" #f)))
         (equal (scan-quoted-string/profile (car row) 0 "'" "`'\\/fnrt" 4)  (cadr row)))
      (equal (scan-quoted-string/profile "x'α'" 1 "'" "" 0)  4)
      (equal (scan-quoted-string/profile "§α§tail" 0 "§" "" 0)  3)
      (equal (scan-quoted-string/profile "'\\u0041'" 0 "'" "" 0)  #f))
    (scenario "compiler admission rejects malformed profiles"
      (each (row) ('((quoted-string-profile "" "" 4) (quoted-string-profile "''" "" 4)
         (quoted-string-profile "'" () 4) (quoted-string-profile "'" "" -1)
         (quoted-string-profile "'" "" 9) (quoted-string-profile "'" "" 1.5)
         (quoted-string-profile "'" ""))) (equal (lexical-expression? row)  #f))
      (equal (lexical-expression? '(quoted-string-profile "§" "" 0))  #t))
    (scenario "generated Scheme grammar keeps source and rejection lossless"
      (each (row) ('(("'α'" . #t) ("`name`" . #t) ("'\\u0041'" . #t)
         ("'\\q'" . #f) ("'\\u123'" . #f) ("'a''b'" . #f)))
         (sequential ((source (car row)) (artifact (parse-source quoted-profile-parser source)))
           (equal (parse-artifact-success? artifact)  (cdr row))
           (equal (parse-artifact-valid? artifact)  #t)
           (equal (parse-artifact-roundtrip artifact)  source)))))
