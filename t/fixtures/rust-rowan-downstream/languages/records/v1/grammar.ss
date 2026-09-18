;;; -*- Gerbil -*-
;;; Downstream-owned record DSL used to qualify the public language-pack API.

(import (only-in :gerbil-parser/rust-rowan-grammar-support deflanguage))
(export +records-language-version+
        +records-syntax-contract+
        records-language-grammar
        records-grammar
        records-parser-ir
        records-parser)

(def +records-language-version+ "v1")
(def +records-syntax-contract+ "downstream-records.v1")

(deflanguage records
  (identity "downstream-records" +records-language-version+
            +records-syntax-contract+)
  (root document)
  (lex
   (whitespace Whitespace (horizontal-whitespace+))
   (newline Newline (newline+))
   (number Number (decimal-digit+))
   (identifier Identifier (identifier))
   (string String (quoted-string "\""))
   (punctuation Punctuation (literals "="))
   (unknown Unknown (fallback)))
  (rules
   (document
    (node Document (repeat assignment)))
   (assignment
    (node Assignment
      (seq
       (field name identifier)
       (literal "=")
       (field value (choice number string identifier))
       (optional newline)))))
  (extras whitespace)
  (keywords)
  (recoveries)
  (conflicts reject)
  (case-insensitive #f))
