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
(export module-source-test module-framing-language-grammar)
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
(def module-source-test
 (test-suite "prepared module framing IR"
  (test-case "the real TLA+ descriptor publishes closed framing data"
   (check (cadr (assq 'module-text (cdr (assq 'lexical-rules (language-grammar-ir tla-plus-sany-candidate-language-grammar)))))
          => module-expression))
  (test-case "framing keeps character boundaries, nested depth and module text stops"
   (for-each (lambda (row)
    (call-with-lexical-source module-plan (car row)
     (lambda () (check (scan-module-text (car row) (cadr row) module-expression) => (caddr row)))))
    module-source-cases))
  (test-case "nested comments and quoted end markers never close a module"
   (for-each (lambda (middle)
    (let* ((prefix (string-append "---- MODULE A ----\n" middle "\n"))
           (source (string-append prefix "still inside\n====\nfooter")))
     (call-with-lexical-source module-plan source
      (lambda () (check (scan-module-text source (string-length prefix) module-expression) => #f)))))
    '("(* ---- MODULE B ----\n==== (* nested ==== *) *)" "\"====\"" "\\* ====\nword")))
  (test-case "long repeated borders remain one indexed interval"
   (let (source (string-append (make-string 100000 #\-) " MODULE A ----\n====\nfooter"))
    (call-with-lexical-source module-plan source
     (lambda ()
      (check (scan-module-text source 0 module-expression) => #f)
      (check (scan-module-text source (- (string-length source) 6) module-expression) => (string-length source))))))
  (test-case "the same data executes in an ordinary lossless parser"
   (for-each (lambda (source)
    (let (artifact (parse-language-source module-framing-language-grammar source))
     (check (parse-artifact-success? artifact) => #t)
     (check (parse-artifact-roundtrip artifact) => source)))
    '("preamble\n---- MODULE A ----\n====\nfooter"
      "---- MODULE A ----\n---- MODULE B ----\n====\ninner\n====\nfooter"
      "---- MODULE A ----\n(* ==== *)\n\"====\"\n====\nfooter")))
  (test-case "an explicit extra name character may itself be numeric"
   (let* ((expression '(module-text "~~~" "UNIT" "!!!" "(*" "*)" "\\*" "0"))
          (plan (prepare-lexical-source-plan (list (list 'text expression)))))
    (for-each (lambda (source expected)
     (call-with-lexical-source plan source (lambda ()
      (check (scan-module-text source 0 expression) => expected))))
     '("~~~ UNIT 000 ~~~" "~~~ UNIT 111 ~~~") '(#f 16))))
  (test-case "malformed framing profiles reject before source preparation"
   (for-each (lambda (expression) (check (lexical-expression? expression) => #f))
    '((module-text "-x" "MODULE" "====" "(*" "*)" "\\*" "_")
      (module-text "----" "" "====" "(*" "*)" "\\*" "_")
      (module-text "----" "MODULE" "====" "(*" "*)" "\\*" "__"))))))
