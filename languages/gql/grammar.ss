;;; -*- Gerbil -*-
;;; Grammar declaration and admitted compiler products for ISO/IEC 39075:2024 GQL generated from the pinned OpenGQL 1.9.0 grammar.

(import (only-in :gerbil-parser/language-support/grammar deflanguage)
        (only-in :gerbil-parser/language-support/antlr4-language
                 antlr4))
(export gql-antlr4-token-bindings gql-antlr4-source gql-syntax gql-grammar gql-bound-grammar-ir gql-parser-ir gql-parser)

(deflanguage gql
 (syntax (antlr4
  (reference "1.9.0"
             "16ea71bd320ad07fd2c46a3066afbaef7d226922")
  (digest
   "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
  (source "grammar-source/GQL.g4")
  (entrypoint gqlProgram)
  (conflicts selective-glr)
  (case-insensitive #t)
  (token-bindings
      ("PARAMETER_NAME" token identifier)
      ("SINGLE_QUOTED_CHARACTER_SEQUENCE" token string)
      ("DOUBLE_QUOTED_CHARACTER_SEQUENCE" token string)
      ("ACCENT_QUOTED_CHARACTER_SEQUENCE" token string)
      ("UNBROKEN_SINGLE_QUOTED_CHARACTER_SEQUENCE" token string)
      ("UNBROKEN_DOUBLE_QUOTED_CHARACTER_SEQUENCE" token string)
      ("UNBROKEN_ACCENT_QUOTED_CHARACTER_SEQUENCE" token string)
      ("BYTE_STRING_LITERAL" token string)
      ("UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_EXACT_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITHOUT_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_EXACT_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITHOUT_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_IN_COMMON_NOTATION_WITH_APPROXIMATE_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_INTEGER_WITH_EXACT_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_INTEGER_WITH_APPROXIMATE_NUMBER_SUFFIX" token number)
      ("UNSIGNED_DECIMAL_INTEGER" token number)
      ("UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATION" token number)
      ("UNSIGNED_DECIMAL_IN_COMMON_NOTATION" token number)
      ("UNSIGNED_HEXADECIMAL_INTEGER" token number)
      ("UNSIGNED_OCTAL_INTEGER" token number)
      ("UNSIGNED_BINARY_INTEGER" token number)
      ("SEPARATED_IDENTIFIER" token identifier)
      ("REGULAR_IDENTIFIER" token identifier)
      ("EXTENDED_IDENTIFIER" token identifier)
      ("DELIMITED_IDENTIFIER" token identifier)
      ("IDENTIFIER_START" token identifier)
      ("IDENTIFIER_EXTEND" token identifier))
  (lex
 (whitespace WhitespaceTrivia (whitespace+))
 (comment CommentTrivia (choice (line-comment "//") (block-comment "/*" "*/")))
 (string StringLiteralToken (quoted-string "\"" "'" "`"))
 (number NumericLiteralToken (number-literal ("0x" "0o" "0b") "_"
                             ("M" "m" "F" "f" "D" "d") #t #t))
 (identifier LexicalIdentifier (identifier))
 (punctuation PunctuationToken (source-literals))
 (unknown UnknownToken (fallback)))
  (extras whitespace comment)))
 (rules))

