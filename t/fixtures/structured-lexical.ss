;;; Shared declaration data for native tests and Rust generation.
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release)
        (only-in :gerbil-parser/languages/tla-plus/grammar
                 tla-proof-start tla-proof-reference tla-identifier))
(export structured-lexical-language-grammar)
(begin
 (deflanguage structured-lexical
  (syntax
   (lexical
    (root entry)
    (lex (space Space (whitespace+))
      (identifier Identifier (text-profile (ref tla-identifier)))
      (proof-step-name ProofStep (text-profile (ref tla-proof-start)))
      (proof-reference ProofReference (text-profile (ref tla-proof-reference))))
    (extras space)))
  (rules (entry (node Entry (choice identifier proof-step-name proof-reference)))))
 (bind-fixture-grammar-release structured-lexical "structured-lexical" "v1" "structured-lexical.v1") )
