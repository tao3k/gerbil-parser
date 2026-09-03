;;; -*- Gerbil -*-
;;; ISO/IEC 39075:2024 GQL generated from the pinned OpenGQL 1.9.0 grammar.

(import :gerbil-parser/languages/support/grammar-source)
(export +gql-standard-reference+
        +gql-standard-edition+
        +gql-opengql-reference-version+
        +gql-opengql-reference-commit+
        +gql-antlr4-digest+
        +gql-syntax-contract-v1+
        +gql-representative-query+
        gql-iso-language-grammar
        gql-iso-grammar
        gql-iso-parser-ir
        gql-iso-parser)

(def +gql-standard-reference+ "ISO/IEC 39075:2024")
(def +gql-standard-edition+ "edition-1-2024-04")
(def +gql-opengql-reference-version+ "1.9.0")
(def +gql-opengql-reference-commit+
  "16ea71bd320ad07fd2c46a3066afbaef7d226922")
(def +gql-antlr4-digest+
  "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
(def +gql-syntax-contract-v1+ "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
(def +gql-representative-query+
  (string-append
   "MATCH (person:Person {name: \"Ada\"}) "
   "OPTIONAL MATCH (person)-[:KNOWS]->(friend:Person) "
   "RETURN person.name AS source, friend.name AS target\n"))

(deflanguage-antlr4-grammar gql-iso
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
