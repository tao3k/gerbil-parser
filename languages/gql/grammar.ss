;;; -*- Gerbil -*-
;;; ISO/IEC 39075:2024 GQL generated from the pinned OpenGQL 1.9.0 grammar.

(import (only-in :gerbil-parser/language-support/antlr4-language
                 deflanguage-antlr4-grammar))
(export +gql-standard-reference+
        +gql-standard-edition+
        +gql-opengql-reference-version+
        +gql-opengql-reference-commit+
        +gql-antlr4-digest+
        +gql-syntax-contract+
        +gql-representative-query+
        gql-language-grammar
        gql-grammar
        gql-bound-grammar-ir
        gql-parser-ir
        gql-parser)

(def +gql-standard-reference+ "ISO/IEC 39075:2024")
(def +gql-standard-edition+ "edition-1-2024-04")
(def +gql-opengql-reference-version+ "1.9.0")
(def +gql-opengql-reference-commit+
  "16ea71bd320ad07fd2c46a3066afbaef7d226922")
(def +gql-antlr4-digest+
  "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
(def +gql-syntax-contract+ "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
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

;;; Corpus declarations share the grammar admission boundary.
(import (only-in :gerbil-parser/language-support
                 defsyntax-corpus defsyntax-fixture syntax-fixture-expected-status))
(export gql-official-fixtures)

(defsyntax-corpus gql-official-fixtures
  (identity "gql" +gql-standard-edition+ +gql-syntax-contract+)
  (accepted
   ("opengql/create-closed-graph-double-colon"
    gql-create-closed-graph-double-colon
    "corpus/reference/create_closed_graph_from_graph_type_double_colon.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-closed-graph-lexical"
    gql-create-closed-graph-lexical
    "corpus/reference/create_closed_graph_from_graph_type_lexical.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-closed-nested-graph-double-colon"
    gql-create-closed-nested-graph-double-colon
    "corpus/reference/create_closed_graph_from_nested_graph_type_double_colon.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-graph"
    gql-create-graph
    "corpus/reference/create_graph.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-schema"
    gql-create-schema
    "corpus/reference/create_schema.gql"
    GqlProgram (CreateSchemaStatement))
   ("opengql/insert-statement"
    gql-insert-statement
    "corpus/reference/insert_statement.gql"
    GqlProgram (InsertStatement))
   ("opengql/match-and-insert"
    gql-match-and-insert
    "corpus/reference/match_and_insert_example.gql"
    GqlProgram (MatchStatement InsertStatement))
   ("opengql/exists-braces"
    gql-exists-braces
    "corpus/reference/match_with_exists_predicate_braces.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/exists-parentheses"
    gql-exists-parentheses
    "corpus/reference/match_with_exists_predicate_parentheses.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/exists-nested-match"
    gql-exists-nested-match
    "corpus/reference/match_with_exists_predicate_nested_match.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/session-current-graph"
    gql-session-current-graph
    "corpus/reference/session_set_graph_to_current_graph.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-current-property-graph"
    gql-session-current-property-graph
    "corpus/reference/session_set_graph_to_current_property_graph.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-property-value"
    gql-session-property-value
    "corpus/reference/session_set_property_as_value.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-time-zone"
    gql-session-time-zone
    "corpus/reference/session_set_time_zone.gql"
    GqlProgram (SessionSetCommand)))
  (rejected))

;;; Pinned source metadata belongs to the grammar declaration.
(import (only-in :gerbil-parser/language-support/grammar-source defsyntax-antlr4-source))
(export gql-antlr4-source)

(defsyntax-antlr4-source gql-antlr4-source
  (identity "gql" "1.9.0"
            "16ea71bd320ad07fd2c46a3066afbaef7d226922")
  (digest
   "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
  (source "grammar-source/GQL.g4"))
