(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;; grammar.ss: candidate author module exercised by the local package control.
(import (only-in :gerbil-parser/language-support/grammar
                 deflanguage defgrammar-syntax))
(export list-study-language-grammar)

(defgrammar-syntax (required-items label item separator)
  (seq (field label item)
       (repeat (seq separator (field label item)))))

(begin
 (deflanguage list-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier))
       (punctuation Punctuation (literals "(" ")" "[" "]" ","))
       (whitespace Whitespace (whitespace+)))
    (extras whitespace)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
      (field form (choice call array))))
   (call
    (node Call
      (field callee identifier) "("
      (optional (required-items argument name ","))
      ")"))
   (array
    (node Array
      "["
      (optional (required-items element name ","))
      "]"))
   (name (node Name (field value identifier)))))
 (bind-fixture-grammar-release list-study "list-study" "v1" "list-study.local.v1") )
