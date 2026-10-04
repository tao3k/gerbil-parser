;;; Strict quoted text is shared lexical data, independent of language callbacks.
(import :std/test
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/src/grammar/lexical-algebra lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan scan-quoted-string/profile)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip))
(export quoted-string-profile-test quoted-profile-language-grammar)

(deflanguage quoted-profile
  (identity "quoted-profile" "v1" "quoted-profile.v1")
  (root document)
  (lex (string String (quoted-string-profile "'" "`'\\/fnrt" 4))
       (name Name (quoted-string-profile "`" "`'\\/fnrt" 4))
       (unknown Unknown (fallback)))
  (rules (document (node SourceFile (field item (choice string name))))))

(def quoted-string-profile-test
  (test-suite "shared quoted string profiles"
    (test-case "strict escape profiles retain valid and invalid endpoints"
      (for-each
       (lambda (row)
         (check (scan-quoted-string/profile (car row) 0 "'" "`'\\/fnrt" 4) => (cadr row)))
       '(("'α'next" 3) ("'a''b'" 3) ("'a\nb'" 5) ("'\\n'" 4)
         ("'\\u0041'" 8) ("'\\u١٢٣٤'" 8) ("'\\u00411'" 9)
         ("'\\x'" #f) ("'\\u123'" #f) ("'\\uGGGG'" #f) ("'\\u²Ⅻ½４'" #f) ("'\\" #f) ("'" #f)))
      (check (scan-quoted-string/profile "x'α'" 1 "'" "" 0) => 4)
      (check (scan-quoted-string/profile "§α§tail" 0 "§" "" 0) => 3)
      (check (scan-quoted-string/profile "'\\u0041'" 0 "'" "" 0) => #f))
    (test-case "compiler admission rejects malformed profiles"
      (for-each
       (lambda (row) (check (lexical-expression? row) => #f))
       '((quoted-string-profile "" "" 4) (quoted-string-profile "''" "" 4)
         (quoted-string-profile "'" () 4) (quoted-string-profile "'" "" -1)
         (quoted-string-profile "'" "" 9) (quoted-string-profile "'" "" 1.5)
         (quoted-string-profile "'" "")))
      (check (lexical-expression? '(quoted-string-profile "§" "" 0)) => #t))
    (test-case "generated Scheme grammar keeps source and rejection lossless"
      (for-each
       (lambda (row)
         (let* ((source (car row)) (artifact (parse-source quoted-profile-parser source)))
           (check (parse-artifact-success? artifact) => (cdr row))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '(("'α'" . #t) ("`name`" . #t) ("'\\u0041'" . #t)
         ("'\\q'" . #f) ("'\\u123'" . #f) ("'a''b'" . #f))))))
