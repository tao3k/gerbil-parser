#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/languages/support/antlr4-source
        :gerbil-parser/languages/gql/iso-39075-2024/source
        :gerbil-parser/src/compiler/lr)

(def tiny-grammar
  (string-append
   "grammar Tiny;\n"
   "options { caseInsensitive = true; }\n"
   "program : MATCH regularIdentifier EOF;\n"
   "regularIdentifier : REGULAR_IDENTIFIER;\n"
   "MATCH : 'MATCH';\n"
   "REGULAR_IDENTIFIER : [a-zA-Z_] [a-zA-Z_0-9]*;\n"
   "SP : [ \\t\\r\\n]+ -> channel(HIDDEN);\n"))

(def antlr4-source-tests
  (test-suite "ANTLR4 grammar source"
    (test-case "a complete grammar catalog preserves parser and lexer owners"
      (let (source (parse-antlr4-source "tiny" "v1" "commit" tiny-grammar))
        (check (antlr4-source-name source) => "Tiny")
        (check (length (antlr4-source-parser-rules source)) => 2)
        (check (length (antlr4-source-lexer-rules source)) => 3)
        (check (antlr4-source-rule source "program") ? antlr4-rule?)
        (check (antlr4-rule-references
                (antlr4-source-rule source "program"))
               => '("MATCH" "regularIdentifier" "EOF"))))
    (test-case "an unresolved parser reference fails closed"
      (check-exception
       (parse-antlr4-source
        "tiny" "v1" "commit"
        "grammar Tiny; program : missingRule EOF;\n")
       true))
    (test-case "the complete OpenGQL 1.9.0 grammar catalog is immutable"
      (check (antlr4-source-digest gql-iso-antlr4-source)
             => +gql-antlr4-digest+)
      (check (antlr4-source-name gql-iso-antlr4-source) => "GQL")
      (check (length (antlr4-source-rules gql-iso-antlr4-source)) => 1018)
      (check (length (antlr4-source-parser-rules gql-iso-antlr4-source))
             => 574)
      (check (length (antlr4-source-lexer-rules gql-iso-antlr4-source))
             => 444)
      (for-each
       (lambda (name)
         (check (antlr4-source-rule gql-iso-antlr4-source name)
                ? antlr4-rule?))
       '("gqlProgram" "valueExpression" "labelExpression"
         "REGULAR_IDENTIFIER")))
    (test-case "all OpenGQL parser productions lower to canonical GrammarExpr v1"
      (let (rules (antlr4-source-parser-grammar-rules gql-iso-antlr4-source))
        (check (length rules) => 574)
        (check (caar rules) => 'gqlProgram)
        (check (> (length
                   (antlr4-source-parser-literals gql-iso-antlr4-source))
                  100)
               => #t)))
    (test-case "the complete OpenGQL grammar compiles through the sole LR owner"
      (let* ((rules
              (antlr4-source-parser-grammar-rules gql-iso-antlr4-source))
             (spec (compile-lr-spec rules 'gqlProgram 'selective-glr)))
        (check (cdr (assq 'schema spec)) => "gerbil-parser.lr-spec.v1")
        (check (> (cdr (assq 'state-count spec)) 0) => #t)))))

(run-tests! antlr4-source-tests)
