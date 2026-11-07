(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Record assignments DSL used to qualify the public language-pack API.

(import (only-in :gerbil-parser/rust-runtime-grammar-support deflanguage))
(export +records-language-version+
        +records-syntax-contract+
        records-language-grammar
        records-grammar
        records-parser-ir
        records-parser)

(def +records-language-version+ "v1")
(def +records-syntax-contract+ "record-assignments.v1")

(begin
 (deflanguage records
  (syntax
   (lexical
    (root document)
    (lex
   (whitespace Whitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (number Number (decimal-digit+))
   (identifier Identifier (identifier))
   (string String (quoted-string "\""))
   (punctuation Punctuation (literals "="))
   (unknown Unknown (fallback)))
    (extras whitespace)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (document
    (node Document (repeat assignment)))
   (assignment
    (node Assignment
      (seq
       (field name identifier)
       (literal "=")
       (field value (choice number string identifier))
       (optional newline))))))
 (bind-fixture-grammar-release records "record-assignments" +records-language-version+ +records-syntax-contract+) )
