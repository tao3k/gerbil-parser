#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import (only-in :clan/poo/object .o)
        (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/gql/parser parse-gql)
        (only-in :gerbil-parser/languages/cypher/parser parse-opencypher)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-roundtrip)
        :gerbil-parser/graph-syntax-support)

(export graph-syntax-syntax-test)

(def Program
  (.o (:: @ GraphSyntaxProgram.)
      match:
      (.o (:: @ GraphSyntaxPath.)
          start: (.o (:: @ GraphSyntaxNode.) binding: 's label: 'Scenario)
          next:
          (.o (:: @ GraphSyntaxStep.) relation: 'HAS_CASE
              target: (.o (:: @ GraphSyntaxNode.) binding: 'c label: 'Case)))
      where:
      (.o (:: @ GraphSyntaxEquals.)
          left: (.o (:: @ GraphSyntaxProperty.) binding: 's property: 'identity)
          right:
          (.o (:: @ GraphSyntaxLiteral.)
              literal-kind: 'string value: "patient's-case"))
      project:
      (.o (:: @ GraphSyntaxProjection.)
          expression:
          (.o (:: @ GraphSyntaxProperty.) binding: 'c property: 'id))))

(def graph-syntax-syntax-test
  (test-suite "shared POO graph syntax"
   (test-case "projects canonical source from the engine graph"
     (check (graph-syntax-program? Program) => #t)
     (check (graph-syntax-program->source Program)
       => "MATCH (s:Scenario)-[:HAS_CASE]->(c:Case)\nWHERE s.identity = 'patient''s-case'\nRETURN c.id\n"))
   (test-case "GQL and Cypher parse the same inherited engine syntax"
     (let (source (graph-syntax-program->source Program))
       (for-each (lambda (parse)
                   (let (artifact (parse source))
                     (check (parse-artifact-success? artifact) => #t)
                     (check (parse-artifact-roundtrip artifact) => source)))
                 (list parse-gql parse-opencypher))))))
