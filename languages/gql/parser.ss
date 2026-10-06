;;; -*- Gerbil -*-
;;; Canonical public parser entry for ISO/IEC 39075:2024 GQL.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.)
        (only-in :gerbil-parser/language-support defsyntax-corpus)
        ./grammar)
(export (import: ./grammar)
        gql-official-fixtures
        gql-language
        parse-gql)

;;; Corpus is a checked entry service, bound to the admitted grammar identity.
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

(deflanguage-parser-loader (gql-language :: self LanguageLoader.)
  (grammar gql-language-grammar)
  (parse parse-gql)
  (slots metadata: (.o grammar-format: 'antlr4 reference-version: +gql-opengql-reference-version+
                             reference-commit: +gql-opengql-reference-commit+
                             source-digest: +gql-antlr4-digest+)
         fixtures: gql-official-fixtures))
