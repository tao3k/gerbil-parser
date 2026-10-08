;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        ./grammar)
(export +gql-standard-edition+ +gql-syntax-contract+ gql-language-grammar (import: ./grammar)
        gql-language
        parse-gql)

(import (only-in :gerbil-parser/language-support/antlr4-source
                 antlr4-source-version antlr4-source-commit antlr4-source-digest))
(export +gql-standard-reference+ +gql-opengql-reference-version+ +gql-opengql-reference-commit+ +gql-antlr4-digest+ +gql-representative-query+)

(def +gql-standard-reference+ "ISO/IEC 39075:2024")

(def +gql-representative-query+
  (string-append
   "MATCH (person:Person {name: \"Ada\"}) "
   "OPTIONAL MATCH (person)-[:KNOWS]->(friend:Person) "
   "RETURN person.name AS source, friend.name AS target\n"))

(def +gql-opengql-reference-version+ (antlr4-source-version gql-antlr4-source))

(def +gql-opengql-reference-commit+ (antlr4-source-commit gql-antlr4-source))

(def +gql-antlr4-digest+ (antlr4-source-digest gql-antlr4-source))

(deflanguage-parser-loader gql-language
  (grammar gql-language-grammar gql-syntax)
  (parse parse-gql)
  (metadata `((language . "gql")
              (version . "edition-1-2024-04")
              (contract . "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
              (grammar-format . antlr4)
              (reference-version . ,+gql-opengql-reference-version+)
              (reference-commit . ,+gql-opengql-reference-commit+)
              (source-digest . ,+gql-antlr4-digest+))))

(def +gql-standard-edition+ (language-metadata-ref (language-parser-entry-ref gql-language 'metadata) 'version))

(def +gql-syntax-contract+ (language-metadata-ref (language-parser-entry-ref gql-language 'metadata) 'contract))
