;;; The Parser prelude supplies all declaration syntax, without private imports.
(deftext-profile number-text (run decimal-digit))
(defgrammar-syntax (one expression) expression)
(deflanguage sample
  (syntax (lexical
    (root document)
    (lex (number Number (decimal-digit+)))
    (extras) (keywords) (recoveries) (conflicts reject)))
  (rules (document (node Document (one (field value number))))))
(export sample-syntax sample-grammar sample-parser-ir sample-parser)
(displayln "PARSER-PRELUDE-GRAMMAR-OK")
