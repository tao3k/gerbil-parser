;;; -*- Gerbil -*-
;;; openGQL 1.9.0 official examples, embedded as immutable syntax fixtures.

(import ../../support/fixture
        ./grammar)
(export gql-iso-official-fixtures)

(defsyntax-corpus gql-iso-official-fixtures
  (identity "gql" +gql-standard-edition+ +gql-syntax-contract-v1+)
  (accepted
   ("opengql/create-closed-graph-double-colon"
    gql-create-closed-graph-double-colon
    "corpus/opengql-1.9.0/create_closed_graph_from_graph_type_double_colon.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-closed-graph-lexical"
    gql-create-closed-graph-lexical
    "corpus/opengql-1.9.0/create_closed_graph_from_graph_type_lexical.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-closed-nested-graph-double-colon"
    gql-create-closed-nested-graph-double-colon
    "corpus/opengql-1.9.0/create_closed_graph_from_nested_graph_type_double_colon.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-graph"
    gql-create-graph
    "corpus/opengql-1.9.0/create_graph.gql"
    GqlProgram (CreateGraphStatement))
   ("opengql/create-schema"
    gql-create-schema
    "corpus/opengql-1.9.0/create_schema.gql"
    GqlProgram (CreateSchemaStatement))
   ("opengql/insert-statement"
    gql-insert-statement
    "corpus/opengql-1.9.0/insert_statement.gql"
    GqlProgram (InsertStatement))
   ("opengql/match-and-insert"
    gql-match-and-insert
    "corpus/opengql-1.9.0/match_and_insert_example.gql"
    GqlProgram (MatchStatement InsertStatement))
   ("opengql/exists-braces"
    gql-exists-braces
    "corpus/opengql-1.9.0/match_with_exists_predicate_braces.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/exists-parentheses"
    gql-exists-parentheses
    "corpus/opengql-1.9.0/match_with_exists_predicate_parentheses.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/exists-nested-match"
    gql-exists-nested-match
    "corpus/opengql-1.9.0/match_with_exists_predicate_nested_match.gql"
    GqlProgram (MatchStatement ExistsPredicate ReturnStatement))
   ("opengql/session-current-graph"
    gql-session-current-graph
    "corpus/opengql-1.9.0/session_set_graph_to_current_graph.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-current-property-graph"
    gql-session-current-property-graph
    "corpus/opengql-1.9.0/session_set_graph_to_current_property_graph.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-property-value"
    gql-session-property-value
    "corpus/opengql-1.9.0/session_set_property_as_value.gql"
    GqlProgram (SessionSetCommand))
   ("opengql/session-time-zone"
    gql-session-time-zone
    "corpus/opengql-1.9.0/session_set_time_zone.gql"
    GqlProgram (SessionSetCommand)))
  (rejected))
