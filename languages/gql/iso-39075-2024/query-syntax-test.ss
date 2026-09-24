#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import (only-in :clan/poo/object .o)
        (only-in :std/test check test-case test-suite)
        ./query-syntax)

(export gql-query-syntax-test)

(def Program
  (.o (:: @ GqlQueryProgram.)
      match:
      (.o (:: @ GqlQueryPath.)
          start: (.o (:: @ GqlQueryNode.) binding: 's label: 'Scenario)
          next:
          (.o (:: @ GqlQueryStep.) relation: 'HAS_CASE
              target: (.o (:: @ GqlQueryNode.) binding: 'c label: 'Case)))
      where:
      (.o (:: @ GqlQueryEquals.)
          left: (.o (:: @ GqlQueryProperty.) binding: 's property: 'identity)
          right:
          (.o (:: @ GqlQueryLiteral.)
              literal-kind: 'string value: "patient's-case"))
      project:
      (.o (:: @ GqlQueryProjection.)
          expression:
          (.o (:: @ GqlQueryProperty.) binding: 'c property: 'id))))

(def gql-query-syntax-test
  (test-suite
   "ISO GQL POO query syntax graph"
   (test-case "projects canonical source from the language-owned graph"
     (check (gql-query-program? Program) => #t)
     (check
      (gql-query-program->source Program)
      =>
      (string-append
       "MATCH (s:Scenario)-[:HAS_CASE]->(c:Case)\n"
       "WHERE s.identity = 'patient''s-case'\n"
       "RETURN c.id\n")))))
