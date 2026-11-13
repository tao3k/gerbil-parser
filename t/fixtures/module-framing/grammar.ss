;;; Shared module-framing grammar for Scheme checks and Rust generation.
(import (only-in :gerbil-parser/language-support/grammar deflanguage)
        (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
(export module-framing-language-grammar)

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
