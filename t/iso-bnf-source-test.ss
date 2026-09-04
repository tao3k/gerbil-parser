#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/languages/support/iso-bnf
        :gerbil-parser/languages/cypher/opencypher-2024-1/grammar)

(def iso-bnf-source-tests
  (test-suite "ISO WG3 BNF grammar source"
    (test-case "the complete openCypher production catalog is immutable"
      (check (iso-bnf-source-language opencypher-2024-1-bnf)
             => "opencypher")
      (check (iso-bnf-source-version opencypher-2024-1-bnf)
             => +opencypher-version+)
      (check (iso-bnf-source-commit opencypher-2024-1-bnf)
             => +opencypher-commit+)
      (check (iso-bnf-source-digest opencypher-2024-1-bnf)
             => +opencypher-bnf-digest+)
      (check (length (iso-bnf-source-productions opencypher-2024-1-bnf))
             => 352)
      (check (iso-bnf-source-production opencypher-2024-1-bnf "program")
             ? iso-bnf-production?)
      (check (iso-bnf-source-production opencypher-2024-1-bnf
                                        "value expression")
             ? iso-bnf-production?)
      (check (iso-bnf-source-production opencypher-2024-1-bnf
                                        "binary digit")
             ? iso-bnf-production?)
      (let* ((program
              (iso-bnf-source-production opencypher-2024-1-bnf "program"))
             (ast (iso-bnf-production-ast program)))
        (check (car ast) => 'choice)
        (check (iso-bnf-production-references program)
               => '("procedure specification" "standalone procedure call")))
      (for-each
       (lambda (production)
         (check (pair? (iso-bnf-production-ast production)) => #t))
       (iso-bnf-source-productions opencypher-2024-1-bnf)))
    (test-case "unresolved production references fail closed"
      (check-exception
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= <missing production>\n")
       true))
    (test-case "duplicate production identities fail closed"
      (check-exception
       (parse-iso-bnf-source
        "example" "v1" "commit"
        "<program> ::= OK\n<program> ::= ALSO_OK\n")
       true))
    (test-case "declared source digest mismatch fails closed"
      (check-exception
       (parse-iso-bnf-source/expected
        "example" "v1" "commit" "sha256:not-the-source"
        "<program> ::= OK\n")
       true))))

(run-tests! iso-bnf-source-tests)
