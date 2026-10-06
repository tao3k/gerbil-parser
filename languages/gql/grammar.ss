;;; -*- Gerbil -*-
;;; Grammar declaration and admitted compiler products for ISO/IEC 39075:2024 GQL generated from the pinned OpenGQL 1.9.0 grammar.

(import (only-in :gerbil-parser/language-support/antlr4-language
                 deflanguage-antlr4-grammar)
        (only-in :gerbil-parser/language-support/antlr4-source
                 antlr4-source-version antlr4-source-commit antlr4-source-digest)
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-version language-grammar-contract))
(export +gql-standard-reference+
        +gql-standard-edition+
        +gql-opengql-reference-version+
        +gql-opengql-reference-commit+
        +gql-antlr4-digest+
        +gql-syntax-contract+
        +gql-representative-query+
        gql-antlr4-source
        gql-language-grammar
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
  (identity "gql" "edition-1-2024-04"
            "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
  (reference "1.9.0"
             "16ea71bd320ad07fd2c46a3066afbaef7d226922")
  (digest
   "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
  (source "grammar-source/GQL.g4")
  (entrypoint gqlProgram)
  (conflicts selective-glr)
  (case-insensitive #t))

;;; Runtime metadata derives from the same admitted declaration and source catalog.
(def +gql-standard-edition+ (language-grammar-version gql-language-grammar))
(def +gql-opengql-reference-version+ (antlr4-source-version gql-antlr4-source))
(def +gql-opengql-reference-commit+ (antlr4-source-commit gql-antlr4-source))
(def +gql-antlr4-digest+ (antlr4-source-digest gql-antlr4-source))
(def +gql-syntax-contract+ (language-grammar-contract gql-language-grammar))

