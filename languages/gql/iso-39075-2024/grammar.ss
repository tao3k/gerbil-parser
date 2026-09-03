;;; -*- Gerbil -*-
;;; Version-pinned ISO GQL grammar exercised by the openGQL 1.9.0 examples.

(import :gerbil-parser/src/language/grammar)
(export +gql-standard-reference+
        +gql-standard-edition+
        +gql-opengql-reference-version+
        +gql-opengql-reference-commit+
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
(def +gql-syntax-contract-v1+ "iso-iec-39075-2024.opengql-examples.v1")
(def +gql-representative-query+
  (string-append
   "MATCH (person:Person {name: \"Ada\"})"
   "-[knows:KNOWS*2]->(friend:Person) "
   "WHERE friend.active = true "
   "OPTIONAL MATCH (friend)-[:WORKS_AT]->(company:Company) "
   "RETURN person.name AS source, friend.name AS target\n"))

(deflanguage-grammar gql-iso
  (identity "gql" +gql-standard-edition+ +gql-syntax-contract-v1+)
  (syntax-kinds
   (GqlProgram node (statement))
   (GqlQuery node (match return))
   (CreateSchemaStatement node (name next))
   (CreateGraphStatement node (name source copy))
   (InsertStatement node (pattern))
   (SessionSetStatement node (target value))
   (MatchClause node (pattern))
   (ReturnClause node (item))
   (ReturnItem node (value alias))
   (ExistsPredicate node (query))
   (GraphPattern node (path))
   (PathPattern node (element))
   (NodePattern node (variable label property))
   (EdgePattern node (variable label property quantifier))
   (GraphTypeBody node (element))
   (NodeType node (name label property))
   (PropertyDefinition node (key type))
   (PropertyMap node (entry))
   (PropertyEntry node (key value))
   (Predicate node (left operator right))
   (PropertyAccess node (root step))
   (TypedLiteral node (type value))
   (ParameterReference node (name))
   (LiteralExpression node (value))
   (Identifier token (text))
   (Number token (text))
   (String token (text))
   (Whitespace token (text))
   (Comment token (text))
   (Punctuation token (text))
   (Unknown token (text)))
  (terminals
   (identifier Identifier)
   (number Number)
   (string String)
   (whitespace Whitespace)
   (comment Comment)
   (punctuation Punctuation)
   (unknown Unknown))
  (lexical-rules
   (whitespace (whitespace+))
   (comment (line-comment "//"))
   (string (quoted-string "\"" "'"))
   (number (number))
   (identifier (identifier))
   (punctuation
    (literals "::" "->" "<-" "(" ")" "[" "]" "{" "}"
              ":" "," "." "=" "-" "*" "/" "$"))
   (unknown (fallback)))
  (rules
   (program
    (alias GqlProgram
      (repeat1 (field statement (reference statement)))))
   (statement
    (choice
     (reference create-schema-statement)
     (reference create-graph-statement)
     (reference match-insert-statement)
     (reference insert-statement)
     (reference query)
     (reference session-set-statement)))
   (create-schema-statement
    (alias CreateSchemaStatement
      (seq
       (literal "CREATE") (literal "SCHEMA")
       (field name (reference catalog-path))
       (optional
        (seq (literal "NEXT")
             (field next (reference create-schema-statement)))))))
   (create-graph-statement
    (alias CreateGraphStatement
      (seq
       (literal "CREATE") (literal "GRAPH")
       (field name (reference graph-reference))
       (field source (reference graph-source))
       (optional
        (seq (literal "AS") (literal "COPY") (literal "OF")
             (field copy (reference graph-reference)))))))
   (graph-source
    (choice
     (literal "ANY")
     (seq (literal "LIKE") (reference graph-reference))
     (seq (literal "TYPED") (token identifier))
     (seq (literal "::") (reference graph-type-reference))
     (reference graph-type-body)
     (token identifier)))
   (graph-type-reference
    (choice (reference graph-type-body) (token identifier)))
   (graph-reference
    (choice (reference catalog-path) (token identifier)))
   (catalog-path
    (seq (literal "/") (token identifier)
         (repeat (seq (literal "/") (token identifier)))))
   (graph-type-body
    (alias GraphTypeBody
      (seq (literal "{")
           (field element (reference node-type))
           (repeat (seq (literal ",")
                        (field element (reference node-type))))
           (literal "}"))))
   (node-type
    (alias NodeType
      (seq
       (literal "(")
       (optional (field name (token identifier)))
       (repeat (seq (literal ":") (field label (token identifier))))
       (optional
        (seq
         (literal "{")
         (field property (reference property-definition))
         (repeat
          (seq (literal ",")
               (field property (reference property-definition))))
         (literal "}")))
       (literal ")"))))
   (property-definition
    (alias PropertyDefinition
      (seq (field key (token identifier))
           (field type (token identifier)))))
   (match-insert-statement
    (seq (field match (reference match-clause))
         (field statement (reference insert-statement))))
   (insert-statement
    (alias InsertStatement
      (seq (literal "INSERT")
           (field pattern (reference graph-pattern)))))
   (query
    (alias GqlQuery
      (seq
       (field match (reference match-clause))
       (repeat (field match (reference match-clause)))
       (field return (reference return-clause)))))
   (match-clause
    (alias MatchClause
      (seq
       (optional (literal "OPTIONAL"))
       (literal "MATCH")
       (field pattern (reference graph-pattern))
       (optional (reference where-clause)))))
   (where-clause
    (seq (literal "WHERE")
         (choice (reference exists-predicate) (reference predicate))))
   (exists-predicate
    (alias ExistsPredicate
      (seq
       (literal "EXISTS")
       (choice
        (seq (literal "(")
             (field query (reference match-clause))
             (literal ")"))
        (seq (literal "{")
             (field query (reference match-clause))
             (optional (reference return-clause))
             (literal "}"))))))
   (return-clause
    (alias ReturnClause
      (seq
       (literal "RETURN")
       (field item (reference return-item))
       (repeat (seq (literal ",") (field item (reference return-item)))))))
   (return-item
    (alias ReturnItem
      (seq
       (field value (reference value-expression))
       (optional (seq (literal "AS") (field alias (token identifier)))))))
   (graph-pattern
    (alias GraphPattern
      (seq
       (field path (reference path-pattern))
       (repeat (seq (literal ",") (field path (reference path-pattern)))))))
   (path-pattern
    (alias PathPattern
      (seq
       (field element (reference node-pattern))
       (repeat
        (seq
         (field element (reference edge-pattern))
         (field element (reference node-pattern)))))))
   (node-pattern
    (alias NodePattern
      (seq
       (literal "(")
       (optional (field variable (token identifier)))
       (repeat (seq (literal ":") (field label (token identifier))))
       (optional (field property (reference property-map)))
       (literal ")"))))
   (edge-pattern
    (alias EdgePattern
      (choice
       (seq (literal "-") (optional (reference edge-detail))
            (choice (literal "->") (literal "-")))
       (seq (literal "<-") (optional (reference edge-detail))
            (literal "-")))))
   (edge-detail
    (seq
     (literal "[")
     (optional (field variable (token identifier)))
     (repeat (seq (literal ":") (field label (token identifier))))
     (optional (field property (reference property-map)))
     (optional (field quantifier
                      (seq (literal "*") (optional (token number)))))
     (literal "]")))
   (property-map
    (alias PropertyMap
      (seq
       (literal "{")
       (optional
        (seq
         (field entry (reference property-entry))
         (repeat (seq (literal ",")
                      (field entry (reference property-entry))))) )
       (literal "}"))))
   (property-entry
    (alias PropertyEntry
      (seq (field key (token identifier)) (literal ":")
           (field value (reference value-expression)))))
   (predicate
    (alias Predicate
      (seq (field left (reference value-expression))
           (field operator (literal "="))
           (field right (reference value-expression)))))
   (value-expression
    (choice
     (reference typed-literal)
     (reference parameter-reference)
     (reference property-access)
     (alias LiteralExpression (field value (token string)))
     (alias LiteralExpression (field value (token number)))))
   (typed-literal
    (alias TypedLiteral
      (seq (field type (token identifier))
           (field value (token string)))))
   (parameter-reference
    (alias ParameterReference
      (seq (literal "$") (field name (token identifier)))))
   (property-access
    (alias PropertyAccess
      (seq (field root (token identifier))
           (repeat (seq (literal ".")
                        (field step (token identifier)))))))
   (session-set-statement
    (alias SessionSetStatement
      (seq
       (literal "SESSION") (literal "SET")
       (choice
        (seq (field target (literal "GRAPH"))
             (field value
                    (choice (literal "CURRENT_GRAPH")
                            (literal "CURRENT_PROPERTY_GRAPH"))))
        (seq (field target (seq (literal "TIME") (literal "ZONE")))
             (field value (token string)))
        (seq (field target (literal "VALUE"))
             (optional (seq (literal "IF") (literal "NOT")
                            (literal "EXISTS")))
             (field value (reference parameter-reference))
             (literal "=")
             (field value (reference value-expression))))))))
  (extras whitespace comment)
  (keywords
   (create "CREATE") (schema "SCHEMA") (graph "GRAPH")
   (match "MATCH") (optional "OPTIONAL") (where "WHERE")
   (return "RETURN") (insert "INSERT") (session "SESSION")
   (set "SET") (as "AS"))
  (parser-entrypoints
   (program parse pure))
  (recoveries
   (program "GERBIL-PARSER-GQL-39075-2024" preserve-source))
  (flow
   (source lexical)
   (lexical gql-iso-statement)
   (gql-iso-statement cst)))
