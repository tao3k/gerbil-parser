;;; -*- Gerbil -*-
;;; Version-pinned ISO GQL graph-pattern core reference grammar.

(import ../modules/parser/config)
(export +gql-standard-reference+
        +gql-standard-edition+
        +gql-syntax-contract-v1+
        +gql-representative-query+
        gql-iso-grammar
        gql-iso-parser-ir
        gql-iso-parser)

(def +gql-standard-reference+ "ISO/IEC 39075:2024")
(def +gql-standard-edition+ "edition-1-2024-04")
(def +gql-syntax-contract-v1+ "iso-iec-39075-2024-graph-pattern-core.v1")
(def +gql-representative-query+
  (string-append
   "MATCH (person:Person {name: \"Ada\"})"
   "-[knows:KNOWS*2]->(friend:Person) "
   "WHERE friend.active = true "
   "OPTIONAL MATCH (friend)-[:WORKS_AT]->(company:Company) "
   "RETURN person.name AS source, friend.name AS target\n"))

(defparser-config gql-iso-role
  gql-iso-grammar
  gql-iso-parser-ir
  gql-iso-parser
  (syntax-kinds
   (GqlQuery node (match return))
   (MatchClause node (pattern))
   (ReturnClause node (item))
   (ReturnItem node (value alias))
   (GraphPattern node (path))
   (PathPattern node (element))
   (NodePattern node (variable label property))
   (EdgePattern node (variable label property quantifier))
   (PropertyMap node (entry))
   (PropertyEntry node (key value))
   (Predicate node (left operator right))
   (PropertyAccess node (root step))
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
   (string (quoted-string "\""))
   (number (decimal-digit+))
   (identifier (identifier))
   (punctuation
    (literals "->" "<-" "(" ")" "[" "]" "{" "}" ":" ","
              "." "=" "-" "*"))
   (unknown (fallback)))
  (rules
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
    (seq (literal "WHERE") (reference predicate)))
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
       (seq
        (literal "-")
        (optional (reference edge-detail))
        (choice (literal "->") (literal "-")))
       (seq
        (literal "<-")
        (optional (reference edge-detail))
        (literal "-")))))
   (edge-detail
    (seq
     (literal "[")
     (optional (field variable (token identifier)))
     (repeat (seq (literal ":") (field label (token identifier))))
     (optional (field property (reference property-map)))
     (optional
      (field quantifier
        (seq (literal "*") (optional (token number)))))
     (literal "]")))
   (property-map
    (alias PropertyMap
      (seq
       (literal "{")
       (optional
        (seq
         (field entry (reference property-entry))
         (repeat
          (seq (literal ",") (field entry (reference property-entry))))))
       (literal "}"))))
   (property-entry
    (alias PropertyEntry
      (seq
       (field key (token identifier))
       (literal ":")
       (field value (reference value-expression)))))
   (predicate
    (alias Predicate
      (seq
       (field left (reference value-expression))
       (field operator (literal "="))
       (field right (reference value-expression)))))
   (value-expression
    (choice
     (reference property-access)
     (alias LiteralExpression (field value (token string)))
     (alias LiteralExpression (field value (token number)))))
   (property-access
    (alias PropertyAccess
      (seq
       (field root (token identifier))
       (repeat (seq (literal ".") (field step (token identifier))))))))
  (extras whitespace comment)
  (keywords
   (match "MATCH")
   (optional "OPTIONAL")
   (where "WHERE")
   (return "RETURN")
   (as "AS"))
  (parser-entrypoints
   (query parse pure))
  (recoveries
   (query "GERBIL-PARSER-GQL-39075-2024" preserve-source))
  (flow
   (source lexical)
   (lexical gql-graph-pattern)
   (gql-graph-pattern cst)))
