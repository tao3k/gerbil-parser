;;; -*- Gerbil -*-
;;; Declarative standards, bound-source identity, lossless syntax and precedence.
(import :gerbil-parser/language-test-support ./parser)
(deflanguage-parser-tests gql-parser-test "ISO/IEC 39075:2024 OpenGQL 1.9.0 syntax"
  (loader gql-language)
  (property "immutable standard and corpus identity" (bindings)
    (equal +gql-standard-reference+ "ISO/IEC 39075:2024")
    (equal +gql-standard-edition+ "edition-1-2024-04")
    (equal +gql-opengql-reference-version+ "1.9.0")
    (equal +gql-opengql-reference-commit+ "16ea71bd320ad07fd2c46a3066afbaef7d226922")
    (equal +gql-syntax-contract+ "iso-iec-39075-2024.opengql-1.9.0-syntax.v1")
    (equal +gql-antlr4-digest+ "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf"))
  (fixture-catalog "official GQL fixture count" (total 14))
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
