#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/language-support
        (only-in :gerbil-parser/languages/gql/grammar
                 gql-parser-ir)
        :gerbil-parser/languages/gql/parser
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        :gerbil-parser/src/compiler/lr
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec))

(def tiny-grammar
  (string-append
   "grammar Tiny;\n"
   "options { caseInsensitive = true; }\n"
   "program : MATCH regularIdentifier EOF;\n"
   "regularIdentifier : REGULAR_IDENTIFIER | MATCH;\n"
   "MATCH : 'MATCH';\n"
   "REGULAR_IDENTIFIER : [a-zA-Z_] [a-zA-Z_0-9]*;\n"
   "SP : [ \\t\\r\\n]+ -> channel(HIDDEN);\n"))

(def indirect-operator-grammar
  (string-append
   "grammar IndirectOperator;\n"
   "program : expression EOF;\n"
   "expression : expression compOp expression"
   " | expression AND expression | IDENTIFIER;\n"
   "compOp : EQUALS | GREATER_THAN;\n"
   "AND : 'AND';\n"
   "EQUALS : '=';\n"
   "GREATER_THAN : '>';\n"
   "IDENTIFIER : [a-zA-Z_] [a-zA-Z_0-9]*;\n"
   "SP : [ \\t\\r\\n]+ -> channel(HIDDEN);\n"))

(deflanguage-parser-tests antlr4-source-tests "ANTLR4 grammar source"
  (engine (exclusive #t))
    (scenario "a complete grammar catalog preserves parser and lexer owners"
      (scope ((source (parse-antlr4-source "tiny" "v1" "commit" tiny-grammar)))
        (equal (antlr4-source-name source)  "Tiny")
        (equal (length (antlr4-source-parser-rules source))  2)
        (equal (length (antlr4-source-lexer-rules source))  3)
        (matches (antlr4-source-rule source "program")  antlr4-rule?)
        (equal (antlr4-rule-references
                (antlr4-source-rule source "program"))
                '("MATCH" "regularIdentifier" "EOF"))
        (equal
         (cadr
          (assq 'regularIdentifier
                (antlr4-source-parser-grammar-rules source
                  '(("REGULAR_IDENTIFIER" token identifier) ("IDENTIFIER" token identifier)))))
          '(alias RegularIdentifier
             (choice
              (precedence left 2 (token identifier))
              (precedence left 1 (literal "MATCH")))))))
    (scenario "token names do not supply implicit language semantics"
      (scope ((source (parse-antlr4-source "tiny" "v1" "commit" tiny-grammar)))
        (raises (antlr4-source-parser-grammar-rules source) true)))
    (scenario "duplicate token bindings fail before lowering"
      (scope ((source (parse-antlr4-source "tiny" "v1" "commit" tiny-grammar)))
        (raises
         (antlr4-source-parser-grammar-rules source
           '(("REGULAR_IDENTIFIER" token identifier)
             ("REGULAR_IDENTIFIER" token number))) true)))
    (property "explicit binding determines a nonliteral token independent of its name"
      (bindings (source (parse-antlr4-source "tiny" "v1" "commit" tiny-grammar)) (rules (antlr4-source-parser-grammar-rules source
                      '(("REGULAR_IDENTIFIER" token number)))))
      (equal (cadr (assq 'regularIdentifier rules))
                '(alias RegularIdentifier
                    (choice (precedence left 2 (token number))
                            (precedence left 1 (literal "MATCH"))))))
    (scenario "an unresolved parser reference fails closed"

      (raises
       (parse-antlr4-source
        "tiny" "v1" "commit"
        "grammar Tiny; program : missingRule EOF;\n")
       true))
    (property "factored operators inherit their direct-left-recursive rank"
      (bindings (source
              (parse-antlr4-source
               "indirect-operator" "v1" "commit"
               indirect-operator-grammar)) (expression
              (cadr
               (assq 'expression
                     (antlr4-source-parser-grammar-rules source
                  '(("REGULAR_IDENTIFIER" token identifier) ("IDENTIFIER" token identifier)))))))
      (equal expression
                '(alias Expression
                    (choice
                     (precedence left 3
                      (sequence
                       (reference expression)
                       (alias CompOp
                        (choice (literal "=") (literal ">")))
                       (reference expression)))
                     (precedence left 2
                      (sequence
                       (reference expression)
                       (literal "AND")
                       (reference expression)))
                     (precedence left 1 (token identifier))))))
    (scenario "the complete OpenGQL 1.9.0 grammar catalog is immutable"
      (equal (parser-ir-ref gql-parser-ir 'schema)
              "gerbil-parser.parser-ir.v1")
      (equal (antlr4-source-digest gql-antlr4-source)
              +gql-antlr4-digest+)
      (equal (antlr4-source-name gql-antlr4-source)  "GQL")
      (equal (length (antlr4-source-rules gql-antlr4-source))  1018)
      (equal (length (antlr4-source-parser-rules gql-antlr4-source))
              574)
      (equal (antlr4-source-parser-grammar-rules gql-antlr4-source gql-antlr4-token-bindings)
              (cdr (assq 'rules gql-grammar)))
      (equal (length (antlr4-source-lexer-rules gql-antlr4-source))
              444)
      (each (name) ('("gqlProgram" "valueExpression" "labelExpression"
         "REGULAR_IDENTIFIER"))
         (matches (antlr4-source-rule gql-antlr4-source name)
                 antlr4-rule?)))
    (property "all OpenGQL parser productions lower to canonical GrammarExpr v1"
      (bindings (rules (antlr4-source-parser-grammar-rules gql-antlr4-source gql-antlr4-token-bindings)))
      (equal (length rules)  574)
      (equal (caar rules)  'gqlProgram)
      (equal (> (length
                   (antlr4-source-parser-literals gql-antlr4-source gql-antlr4-token-bindings))
                  100)
                #t))
    (property "the complete OpenGQL grammar compiles through the sole LR owner"
      (bindings (rules
              (antlr4-source-parser-grammar-rules gql-antlr4-source gql-antlr4-token-bindings)) (spec (compile-lr-spec rules 'gqlProgram 'selective-glr)))
      (equal (lr-spec-ref spec 'schema)  "gerbil-parser.lr-spec.v1")
      (equal (> (lr-spec-ref spec 'state-count) 0)  #t)))


;; gxtest discovers only exported names ending in -test.
(def antlr4-source-test antlr4-source-tests)
(export antlr4-source-test)
