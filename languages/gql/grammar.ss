;;; -*- Gerbil -*-
;;; Grammar declaration and admitted compiler products for ISO/IEC 39075:2024 GQL generated from the pinned OpenGQL 1.9.0 grammar.

(import (only-in :gerbil-parser/language-support/antlr4-language
                 deflanguage-antlr4-grammar)
        (only-in :gerbil-parser/language-support/antlr4-source
                 antlr4-source-version antlr4-source-commit antlr4-source-digest))
(export +gql-standard-reference+
        +gql-opengql-reference-version+
        +gql-opengql-reference-commit+
        +gql-antlr4-digest+
        +gql-representative-query+
        gql-antlr4-token-bindings
        gql-antlr4-source
        gql-syntax
        gql-grammar
        gql-bound-grammar-ir
        gql-parser-ir
        gql-parser)

(def +gql-standard-reference+ "ISO/IEC 39075:2024")
(def +gql-representative-query+
  (string-append
   "MATCH (person:Person {name: \"Ada\"}) "
   "OPTIONAL MATCH (person)-[:KNOWS]->(friend:Person) "
   "RETURN person.name AS source, friend.name AS target\n"))

(deflanguage-antlr4-grammar gql
  (reference "1.9.0"
             "16ea71bd320ad07fd2c46a3066afbaef7d226922")
  (digest
   "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
  (source "grammar-source/GQL.g4")
  (entrypoint gqlProgram)
  (conflicts selective-glr)
  (case-insensitive #t)
  (lexical-profile
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
    (syntax-kinds
      (LexicalIdentifier token (text)) (NumericLiteralToken token (text))
      (StringLiteralToken token (text)) (WhitespaceTrivia token (text))
      (CommentTrivia token (text)) (PunctuationToken token (text))
      (UnknownToken token (text)))
    (terminals
      (identifier LexicalIdentifier) (number NumericLiteralToken)
      (string StringLiteralToken) (whitespace WhitespaceTrivia)
      (comment CommentTrivia) (punctuation PunctuationToken) (unknown UnknownToken))
    (lexical-rules
      (whitespace (whitespace+))
      (comment (choice (line-comment "//") (block-comment "/*" "*/")))
      (string (quoted-string "\"" "'" "`"))
      (number (number-literal ("0x" "0o" "0b") "_"
                             ("M" "m" "F" "f" "D" "d") #t #t))
      (identifier (identifier))
      (punctuation (source-literals))
      (unknown (fallback)))
    (extras whitespace comment)))

;;; Runtime metadata derives from the same admitted declaration and source catalog.
(def +gql-opengql-reference-version+ (antlr4-source-version gql-antlr4-source))
(def +gql-opengql-reference-commit+ (antlr4-source-commit gql-antlr4-source))
(def +gql-antlr4-digest+ (antlr4-source-digest gql-antlr4-source))

