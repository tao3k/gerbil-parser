#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/reference/gql-iso-39075-2024
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser)

(def (cst-node-kinds value)
  (cond
   ((syntax-node? value)
    (cons (syntax-node-kind value)
          (apply append (map cst-node-kinds (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map cst-node-kinds (syntax-field-children value))))
   (else '())))

(def gql-iso-tests
  (test-suite "ISO/IEC 39075:2024 graph-pattern core"
    (test-case "the GQL standard identity is immutable"
      (check +gql-standard-reference+ => "ISO/IEC 39075:2024")
      (check +gql-standard-edition+ => "edition-1-2024-04")
      (check +gql-syntax-contract-v1+
             => "iso-iec-39075-2024-graph-pattern-core.v1"))
    (test-case "the generic machine parses multi-clause GQL losslessly"
      (let* ((artifact (parse-source gql-iso-parser +gql-representative-query+))
             (root (parse-artifact->cst artifact))
             (kinds (cst-node-kinds root)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => +gql-representative-query+)
        (check (syntax-node-kind root) => 'GqlQuery)
        (check (= (length (filter (cut eq? <> 'MatchClause) kinds)) 2) => #t)
        (check (member 'NodePattern kinds) ? values)
        (check (member 'EdgePattern kinds) ? values)
        (check (member 'PropertyMap kinds) ? values)
        (check (member 'Predicate kinds) ? values)
        (check (member 'ReturnClause kinds) ? values)))
    (test-case "malformed graph patterns reject as one typed artifact"
      (for-each
       (lambda (source)
         (let (artifact (parse-source gql-iso-parser source))
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)
           (check (length (parse-artifact-ref artifact 'diagnostics)) => 1)))
       '("MATCH (n RETURN n\n"
         "MATCH (n)-[r]-> RETURN n\n"
         "MATCH (n {name: \"Ada\"}) WHERE n.name RETURN n\n")))))

(run-tests! gql-iso-tests)
