#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import (only-in :clan/poo/object .o)
        (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/gql/parser parse-gql)
        (only-in :gerbil-parser/languages/cypher/parser parse-opencypher)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-roundtrip)
        :gerbil-parser/graph-syntax-support)


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

(deflanguage-parser-tests graph-syntax-syntax-test "shared POO graph syntax"
  (engine)
   (property "projects canonical source from the engine graph"
      (bindings)
      (equal (graph-syntax-program? Program)  #t)
      (equal (graph-syntax-program->source Program)
        "MATCH (s:Scenario)-[:HAS_CASE]->(c:Case)\nWHERE s.identity = 'patient''s-case'\nRETURN c.id\n"))
   (scenario "GQL and Cypher parse the same inherited engine syntax"
     (scope ((source (graph-syntax-program->source Program)))
       (each (parse) ((list parse-gql parse-opencypher))
                   (scope ((artifact (parse source)))
                     (equal (parse-artifact-success? artifact)  #t)
                     (equal (parse-artifact-roundtrip artifact)  source))))))
