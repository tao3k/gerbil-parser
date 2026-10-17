(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Source framing is closed lexical data with bounded immutable indexes.
(import :std/test
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/src/grammar/lexical-algebra lexical-expression?)
        (only-in :gerbil-parser/src/runtime/lexical-source
                 scan-module-text prepare-lexical-source-plan call-with-lexical-source)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-roundtrip)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-sany-candidate-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-ir)
        (only-in "module-source-cases.ss" module-source-cases))
(export  module-framing-language-grammar)
(def module-expression '(module-text "----" "MODULE" "====" "(*" "*)" "\\*" "_"))
(def module-plan (prepare-lexical-source-plan (list (list 'text module-expression))))
(begin
 (deflanguage module-framing
  (syntax
   (lexical
    (root document)
    (lex (text ModuleText (module-text "----" "MODULE" "====" "(*" "*)" "\\*" "_"))
      (border Border (character-run "-" 4)) (end End (character-run "=" 4))
      (name Name (identifier)) (string String (quoted-string "\""))
      (space Space (whitespace+))
      (comment Comment (choice (line-comment "\\*") (nested-block-comment "(*" "*)"))))
    (extras space comment)))
  (rules
  (document (node Document (seq (optional text) (repeat1 (seq (reference unit) (optional text))))))
  (unit (node Unit (seq border (literal "MODULE") (field name name) border
                       (repeat (choice name string (reference unit))) end)))))
 (bind-fixture-grammar-release module-framing "module-framing" "v1" "module-framing.v1") )
(deflanguage-parser-tests module-source-test "prepared module framing IR"
  (engine)
  (property "the real TLA+ descriptor publishes closed framing data"
      (bindings)
      (equal (cadr (assq 'module-text (cdr (assq 'lexical-rules (language-grammar-ir tla-plus-sany-candidate-language-grammar)))))
           module-expression))
  (scenario "framing keeps character boundaries, nested depth and module text stops"
   (each (row) (module-source-cases)
    (using (call-with-lexical-source module-plan (car row)) () (equal (scan-module-text (car row) (cadr row) module-expression)  (caddr row)))))
  (scenario "nested comments and quoted end markers never close a module"
   (each (middle) ('("(* ---- MODULE B ----\n==== (* nested ==== *) *)" "\"====\"" "\\* ====\nword"))
    (sequential ((prefix (string-append "---- MODULE A ----\n" middle "\n"))
           (source (string-append prefix "still inside\n====\nfooter")))
     (using (call-with-lexical-source module-plan source) () (equal (scan-module-text source (string-length prefix) module-expression)  #f)))))
  (scenario "long repeated borders remain one indexed interval"
   (scope ((source (string-append (make-string 100000 #\-) " MODULE A ----\n====\nfooter")))
    (using (call-with-lexical-source module-plan source) ()
      (equal (scan-module-text source 0 module-expression)  #f)
      (equal (scan-module-text source (- (string-length source) 6) module-expression)  (string-length source)))))
  (scenario "the same data executes in an ordinary lossless parser"
   (each (source) ('("preamble\n---- MODULE A ----\n====\nfooter"
      "---- MODULE A ----\n---- MODULE B ----\n====\ninner\n====\nfooter"
      "---- MODULE A ----\n(* ==== *)\n\"====\"\n====\nfooter"))
    (scope ((artifact (parse-language-source module-framing-language-grammar source)))
     (equal (parse-artifact-success? artifact)  #t)
     (equal (parse-artifact-roundtrip artifact)  source))))
  (scenario "an explicit extra name character may itself be numeric"
   (sequential ((expression '(module-text "~~~" "UNIT" "!!!" "(*" "*)" "\\*" "0"))
          (plan (prepare-lexical-source-plan (list (list 'text expression)))))
    (each (source expected) ('("~~~ UNIT 000 ~~~" "~~~ UNIT 111 ~~~") '(#f 16))
     (using (call-with-lexical-source plan source) ()
      (equal (scan-module-text source 0 expression)  expected)))))
  (scenario "malformed framing profiles reject before source preparation"
   (each (expression) ('((module-text "-x" "MODULE" "====" "(*" "*)" "\\*" "_")
      (module-text "----" "" "====" "(*" "*)" "\\*" "_")
      (module-text "----" "MODULE" "====" "(*" "*)" "\\*" "__"))) (equal (lexical-expression? expression)  #f))))
