;;; -*- Gerbil -*-
;;; Declarative standards, bound-source identity, lossless syntax and precedence.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-grammar language-grammar-ir language-grammar-machine)
        (only-in :gerbil-parser/language-support/entry language-parser-entry-ref)
        (only-in :gerbil-parser/language-support/fixture syntax-fixture-id syntax-fixture-source)
        (only-in :clan/poo/object .ref)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development
                 deflanguage-development-loader LanguageDevelopmentLoader.)
        ./parser)

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


(deflanguage-development-loader (gql-test-language :: self LanguageDevelopmentLoader.)
  (grammar gql-language-grammar)
  (parse parse-gql-test)
  (slots fixtures: gql-official-fixtures))

(export gql-official-fixtures gql-test-language)

(deflanguage-parser-tests gql-parser-test "ISO/IEC 39075:2024 OpenGQL 1.9.0 syntax"
  (loader gql-test-language)
  (property "immutable standard and corpus identity" (bindings)
    (equal +gql-standard-reference+ "ISO/IEC 39075:2024")
    (equal +gql-standard-edition+ "edition-1-2024-04")
    (equal +gql-opengql-reference-version+ "1.9.0")
    (equal +gql-opengql-reference-commit+ "16ea71bd320ad07fd2c46a3066afbaef7d226922")
    (equal +gql-syntax-contract+ "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
    (equal +gql-antlr4-digest+ "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf"))
  (fixture-catalog "official GQL fixture count" (total 14))
  (property "published products and checked entry share one declaration" (bindings)
    (equal (eq? (language-grammar-grammar gql-language-grammar) gql-grammar) #t)
    (equal (eq? (language-grammar-ir gql-language-grammar) gql-parser-ir) #t)
    (equal (eq? (language-grammar-machine gql-language-grammar) gql-parser) #t)
    (equal (language-parser-entry-ref gql-language 'language) "gql")
    (equal (language-parser-entry-ref gql-language 'contract)
           "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
    (equal (map syntax-fixture-id (.ref gql-test-language 'fixtures))
           (map syntax-fixture-id gql-official-fixtures))
    (equal (map syntax-fixture-source (.ref gql-test-language 'fixtures))
           (map syntax-fixture-source gql-official-fixtures)))
  (parser-ir "generated Parser IR identity" gql-parser-ir
    (rule-count 574) (materialization 'aot-expansion)
    (conflict-policy 'selective-glr) (case-insensitive? #t))
  (bound-ir "bound catalog identity" gql-bound-grammar-ir (binding-count 1176))
  (bound-rules "source-bound root rule and ordered references" gql-bound-grammar-ir
    (gqlProgram
      (binding-id '((package . gerbil-parser) (grammar . gql-grammar)
                    (namespace . rule) (name . gqlProgram)))
      (source path "grammar-source/GQL.g4")
      (source location "grammar-source/GQL.g4#gqlProgram")
      (reference-names '(programActivity sessionCloseCommand sessionCloseCommand))))
  (accepted "multi-clause query retains public syntax" +gql-representative-query+
    (root GqlProgram) (counts (MatchStatement 2))
    (nodes NodePattern EdgePattern ElementPropertySpecification PropertyKeyValuePair ReturnStatement))
  (accepted "case-insensitive keywords retain source" "match (n) return n\n")
  (accepted "comparison operands bind before conjunction"
    "MATCH (n) WHERE n.score > 2 AND n.active = TRUE RETURN n"
    (subtree (node ValueExpression (lexemes "AND")
      (children (node ValueExpression (lexemes ">"))
                (node ValueExpression (lexemes "=")))))
    (without-subtree (node ValueExpression (lexemes "AND")
      (children (node ValueExpression (lexemes "="))
                (node ValueExpression (lexemes ">"))))))
  (accepted-many "delimited identifiers and numeric profile"
    '("MATCH (n) RETURN n AS \"a\"\"b\"\n"
      "RETURN 0xFF, 0o77, 0b101, .5, 1., 2.0M\n"))
  (fixtures "all official examples retain their required structure" (status accepted))
  (rejected-many "malformed graph patterns yield one diagnostic"
    '("MATCH\n" "CREATE\n" "SESSION\n") (diagnostics 1)))
