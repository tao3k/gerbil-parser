;;; -*- Gerbil -*-
;; grammar.ss: candidate author module exercised by the local package control.
(import (only-in :gerbil-parser/language-support/grammar
                 deflanguage defgrammar-syntax))
(export list-study-language-grammar)

(defgrammar-syntax (required-items label item separator)
  (seq (field label item)
       (repeat (seq separator (field label item)))))

(deflanguage list-study
  (identity "list-study" "v1" "list-study.local.v1")
  (root source-file)
  (lex (identifier Identifier (identifier))
       (punctuation Punctuation (literals "(" ")" "[" "]" ","))
       (whitespace Whitespace (whitespace+)))
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
   (name (node Name (field value identifier))))
  (extras whitespace) (keywords) (recoveries)
  (conflicts reject) (case-insensitive #f))
