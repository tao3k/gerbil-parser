#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(deflanguage-parser-tests grammar-composition-lowering-test "grammar composition lowering"
  (engine)
    (property "parser IR preserves declared flow"
      (bindings)
      (equal (parser-ir-ref arithmetic-parser-ir 'schema)
              "gerbil-parser.parser-ir.v1")
      (equal (parser-ir-ref arithmetic-parser-ir 'flow)
              '((source lexical) (lexical parser) (parser cst))))
    (property "deterministic LALR(1) compilation admits declared precedence"
      (bindings (lr-spec (parser-ir-ref arithmetic-parser-ir 'lr-spec)))
      (equal (lr-spec-ref lr-spec 'schema)
                "gerbil-parser.lr-spec.v1")
      (equal (lr-spec-ref lr-spec 'algorithm)
                'lalr1-lr0-fixed-point-v1)
      (equal (lr-spec-ref lr-spec 'lr0-state-visit-count)
                (lr-spec-ref lr-spec 'state-count))
      (equal (> (lr-spec-ref lr-spec 'lookahead-item-visit-count) 0)
                #t)
      (equal (> (lr-spec-ref lr-spec 'state-count) 0)  #t))
    (property "language declarations materialize LR tables during AOT expansion"
      (bindings)
      (equal (parser-ir-ref arithmetic-parser-ir 'materialization)
              'aot-expansion)
      (equal (pair? (parser-ir-ref arithmetic-parser-ir 'lr-spec))  #t))
    (property "canonical parser IR including LALR tables is deterministic"
      (bindings)
      (equal (parser-ir-canonical (compile-parser arithmetic-grammar))

             (parser-ir-canonical (compile-parser arithmetic-grammar))))
    (scenario "grammar algebra preserves productions and entrypoint"
      (equal (parser-ir-ref arithmetic-parser-ir 'root-rule)  'source-file)
      (equal (length (parser-ir-ref arithmetic-parser-ir 'terminals))  5)
      (equal (length (parser-ir-ref arithmetic-parser-ir 'lexical-rules))  5)
      (equal (length (parser-ir-ref arithmetic-parser-ir 'rules))  6)
      (equal (parser-ir-ref arithmetic-parser-ir 'extras)
              '((whitespace)))
      (sequential ((rules (parser-ir-ref arithmetic-parser-ir 'rules))
             (source-expression (cadr (assq 'source-file rules))))
        (equal (grammar-expression-kind source-expression)  'alias)
        (equal source-expression
                '(alias SourceFile
                     (field expression (reference expression))))))
    (scenario "nullable repetition is rejected at construction"
      (raises
       (grammar-expression
        (repeat (optional (token identifier))))
       true))
    (scenario "lexical literals reject empty spellings"
      (raises
       (lexical-expression (literals ""))
       true))
    (scenario "every terminal requires exactly one lexical rule"
      (raises
       (compile-composed missing-lexical-rule-grammar)
       true))
    (scenario "unresolved production references are rejected"
      (raises
       (compile-composed unresolved-reference-grammar)
       true))
    (scenario "terminals require token syntax kinds"
      (raises
       (compile-composed invalid-terminal-kind-grammar)
       true))
    (property "precedence-bearing grammars compile to the LR owner"
      (bindings)
      (equal (lr-spec-ref
              (parser-ir-ref
               (compile-composed unsupported-precedence-grammar)
               'lr-spec)
              'schema)
              "gerbil-parser.lr-spec.v1"))
    (property "unresolved LR ambiguity fails closed"
      (bindings)
      (equal (condition-message
              (lambda ()
                (compile-lr-spec ambiguous-left-recursive-rules
                                 'source-file)))
              "unresolved shift/reduce conflict requires precedence"))
)
