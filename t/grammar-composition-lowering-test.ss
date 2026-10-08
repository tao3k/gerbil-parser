#!/usr/bin/env gxi
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-

(import :std/test
        (only-in :clan/poo/object .o)
        :gerbil-parser/src/grammar/algebra
        :gerbil-parser/src/grammar/lexical-algebra
        :gerbil-parser/src/compiler/normalize
        :gerbil-parser/src/compiler/parser-ir
        :gerbil-parser/src/compiler/lr
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        :gerbil-parser/src/language/grammar
        :gerbil-parser/src/modules/parser/interface
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/funcs vector-intern-map)
        (only-in :gerbil-parser/src/runtime/incremental
                 apply-edit make-edit make-incremental-session
                 incremental-session-artifact parse-incremental-session
                 parse-source/incremental)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-checkpoint? lr-checkpoint-deterministic-actions
                 lr-checkpoint-deterministic-shifts
                 lr-checkpoint-remaining-token-count
                 lr-checkpoint-advance lr-checkpoint-resume
                 lr-initial-checkpoint lr-parse lr-parse/receipt lr-prepare
                 lr-runtime-lexical-mode-catalog)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/compiler/machine
        :gerbil-parser/languages/arithmetic/parser)
(import :gerbil-parser/t/fixtures/grammar-composition-models)

(def grammar-composition-lowering-test
  (test-suite "grammar composition lowering"
    (test-case "parser IR preserves declared flow"
      (check (parser-ir-ref arithmetic-parser-ir 'schema)
             => "gerbil-parser.parser-ir.v1")
      (check (parser-ir-ref arithmetic-parser-ir 'flow)
             => '((source lexical) (lexical parser) (parser cst))))
    (test-case "deterministic LALR(1) compilation admits declared precedence"
      (let (lr-spec (parser-ir-ref arithmetic-parser-ir 'lr-spec))
        (check (lr-spec-ref lr-spec 'schema)
               => "gerbil-parser.lr-spec.v1")
        (check (lr-spec-ref lr-spec 'algorithm)
               => 'lalr1-lr0-fixed-point-v1)
        (check (lr-spec-ref lr-spec 'lr0-state-visit-count)
               => (lr-spec-ref lr-spec 'state-count))
        (check (> (lr-spec-ref lr-spec 'lookahead-item-visit-count) 0)
               => #t)
        (check (> (lr-spec-ref lr-spec 'state-count) 0) => #t)))
    (test-case "language declarations materialize LR tables during AOT expansion"
      (check (parser-ir-ref arithmetic-parser-ir 'materialization)
             => 'aot-expansion)
      (check (pair? (parser-ir-ref arithmetic-parser-ir 'lr-spec)) => #t))
    (test-case "canonical parser IR including LALR tables is deterministic"
      (check (parser-ir-canonical (compile-parser arithmetic-grammar))
             =>
             (parser-ir-canonical (compile-parser arithmetic-grammar))))
    (test-case "grammar algebra preserves productions and entrypoint"
      (check (parser-ir-ref arithmetic-parser-ir 'root-rule) => 'source-file)
      (check (length (parser-ir-ref arithmetic-parser-ir 'terminals)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'lexical-rules)) => 5)
      (check (length (parser-ir-ref arithmetic-parser-ir 'rules)) => 6)
      (check (parser-ir-ref arithmetic-parser-ir 'extras)
             => '((whitespace)))
      (let* ((rules (parser-ir-ref arithmetic-parser-ir 'rules))
             (source-expression (cadr (assq 'source-file rules))))
        (check (grammar-expression-kind source-expression) => 'alias)
        (check source-expression
               => '(alias SourceFile
                     (field expression (reference expression))))))
    (test-case "nullable repetition is rejected at construction"
      (check-exception
       (grammar-expression
        (repeat (optional (token identifier))))
       true))
    (test-case "lexical literals reject empty spellings"
      (check-exception
       (lexical-expression (literals ""))
       true))
    (test-case "every terminal requires exactly one lexical rule"
      (check-exception
       (compile-composed missing-lexical-rule-grammar)
       true))
    (test-case "unresolved production references are rejected"
      (check-exception
       (compile-composed unresolved-reference-grammar)
       true))
    (test-case "terminals require token syntax kinds"
      (check-exception
       (compile-composed invalid-terminal-kind-grammar)
       true))
    (test-case "precedence-bearing grammars compile to the LR owner"
      (check (lr-spec-ref
              (parser-ir-ref
               (compile-composed unsupported-precedence-grammar)
               'lr-spec)
              'schema)
             => "gerbil-parser.lr-spec.v1"))
    (test-case "unresolved LR ambiguity fails closed"
      (check (condition-message
              (lambda ()
                (compile-lr-spec ambiguous-left-recursive-rules
                                 'source-file)))
             => "unresolved shift/reduce conflict requires precedence"))
))
(export grammar-composition-lowering-test)
