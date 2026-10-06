;;; -*- Gerbil -*-
;;; Matched complete package variant; authoring still uses the three DSL interfaces.
(import (only-in :gerbil-parser/language-support deflanguage defgrammar-syntax))
(export list-parameter-inferred-language-grammar)
(defgrammar-syntax (required-items label item separator)
  (seq (field label item) (repeat (seq separator (field label item)))))
(deflanguage list-parameter-inferred
  (identity "list-parameter-inferred" "v1" "list-parameter-inferred.local.v1")
  (root source-file)
  (lex (identifier Identifier (identifier))
       (punctuation Punctuation (literals "(" ")" "[" "]" ","))
       (whitespace Whitespace (whitespace+)))
  (rules
   (source-file
    (node SourceFile
      (field form (choice (reference call) (reference array)))))
   (call
    (node Call
      (seq (field callee (token identifier)) (literal "(")
           (optional
            (required-items parameter (reference name) (literal ",")))
           (literal ")"))))
   (array
    (node Array
      (seq (literal "[")
           (optional
            (required-items element (reference name) (literal ",")))
           (literal "]"))))
   (name (node Name (field value (token identifier)))))
  (extras whitespace) (keywords) (recoveries)
  (conflicts reject) (case-insensitive #f))
