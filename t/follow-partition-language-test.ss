#!/usr/bin/env gxi
;;; Native module Fixture selects the existing Core Case budget for this Suite.
(import (only-in ./fixtures/case-profile test-setup! test-cleanup!))
(export test-setup! test-cleanup!)
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Grammar IR qualification of the optional Scheme LR construction.

(import (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/language-support/development
                 language-loader-fixtures language-loader-fixture-count)
        (only-in :gerbil-parser/language-support/entry language-parser-entry-ref)
        :std/test
        (only-in :yuanxu/observability/testing-case yuanxu-test-case)
        (only-in :gerbil-parser/languages/arithmetic/grammar
                 arithmetic-grammar arithmetic-parser)
        (only-in :gerbil-parser/languages/hcl/grammar
                 hcl-grammar hcl-parser
                 hcl-parser-ir)

        (only-in :gerbil-parser/language-support/fixture
                 syntax-fixture-id syntax-fixture-source)
        (only-in :gerbil-parser/src/compiler/parser-ir
                 compile-parser parser-ir-ref)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 genuine-reduce-conflict-rules lr1-not-lalr-rules
                 precedence-expression-rules))

(def (grammar-ir rules)
  (list (cons 'schema "gerbil-parser.grammar-ir.v1")
        (cons 'grammar 'follow-partition-witness)
        (cons 'syntax-kinds '((SourceFile node ())))
        (cons 'terminals '())
        (cons 'lexical-rules '())
        (cons 'rules rules)
        (cons 'extras '())
        (cons 'keywords '())
        (cons 'parser-entrypoints '((source-file parse pure)))
        (cons 'recoveries '())
        (cons 'flow '((source lexical) (lexical cst)))
        (cons 'conflict-policy 'reject)
        (cons 'case-insensitive? #f)))

(def (character-tokens source)
  (let loop ((index 0) (tokens '()))
    (if (= index (string-length source))
      (reverse tokens)
      (loop (+ index 1)
            (cons (make-token 'punctuation
                              (string (string-ref source index))
                              index (+ index 1))
                  tokens)))))

(def (parsed-root spec source)
  (let-values (((root rest) (lr-parse spec (character-tokens source))))
    (unless (null? rest)
      (error "Grammar IR parser left a token suffix" source rest))
    root))

(def (parsed-language-root spec tokens)
  (let-values (((root rest) (lr-parse spec tokens)))
    (unless (null? rest)
      (error "language parser left a token suffix" rest))
    root))

(defparser-check (check-grammar-ir-equivalence rules sources)
  (sequential ((grammar (grammar-ir rules))
         (canonical
          (parser-ir-ref (compile-parser grammar 'canonical-lr1) 'lr-spec))
         (direct
          (parser-ir-ref (compile-parser grammar 'follow-partition-lr1)
                         'lr-spec)))
    (equal (lr-spec-ref direct 'algorithm)  'follow-partition-lr1-v1)
    (equal (> (lr-spec-ref direct 'state-count) 0)  #t)
    (each (source) (sources)
       (equal (equal? (parsed-root canonical source)
                      (parsed-root direct source))
               #t))))

(deflanguage-parser-tests follow-partition-language-test "follow partition through Grammar IR"
  (engine)
    (profiled "default Grammar IR construction remains LALR"
      (scope ((grammar (grammar-ir precedence-expression-rules)))
        (equal (parser-ir-ref (compile-parser grammar) 'lr-spec)
                (parser-ir-ref (compile-parser grammar 'lalr) 'lr-spec))))
    (scenario "explicit adaptive construction selects by conflict"
      (sequential ((ordinary (compile-parser
                        (grammar-ir precedence-expression-rules)
                        'lalr-then-follow))
             (ordinary-spec (parser-ir-ref ordinary 'lr-spec))
             (split (compile-parser
                     (grammar-ir lr1-not-lalr-rules)
                     'lalr-then-follow))
             (split-spec (parser-ir-ref split 'lr-spec))
             (reference (parser-ir-ref
                         (compile-parser (grammar-ir lr1-not-lalr-rules)
                                         'canonical-lr1)
                         'lr-spec)))
        (equal (lr-spec-ref ordinary-spec 'algorithm)
                'lalr1-lr0-fixed-point-v1)
        (equal ordinary-spec
                (parser-ir-ref
                   (compile-parser (grammar-ir precedence-expression-rules)
                                   'lalr)
                   'lr-spec))
        (equal (lr-spec-ref split-spec 'algorithm)
                'follow-partition-lr1-v1)
        (each (source) ('("acd" "bce" "ace" "bcd"))
           (equal (parsed-root split-spec source)
                   (parsed-root reference source)))))
    (scenario "adaptive construction preserves genuine conflict rejection"
      (raises
       (compile-lr-spec genuine-reduce-conflict-rules 'source-file
                        'reject #f 'lalr-then-follow)
       true))
    (property "selective GLR policy keeps its LALR fork route"
      (bindings (spec
            (compile-lr-spec genuine-reduce-conflict-rules 'source-file
                             'selective-glr #f 'lalr-then-follow)))
      (equal (lr-spec-ref spec 'algorithm)
                'lalr1-lr0-fixed-point-v1))
    (profiled "LR(1)-only grammar keeps its parse products"
      (verify check-grammar-ir-equivalence lr1-not-lalr-rules '("acd" "bce" "ace" "bcd")))
    (profiled "precedence grammar keeps its parse products"
      (verify check-grammar-ir-equivalence precedence-expression-rules '("a" "a+a" "a*a" "a+a*a" "a*a+a")))
    (profiled "generated arithmetic language keeps parse products"
      (sequential ((canonical
              (parser-ir-ref
               (compile-parser arithmetic-grammar 'canonical-lr1)
               'lr-spec))
             (direct
              (parser-ir-ref
               (compile-parser arithmetic-grammar 'follow-partition-lr1)
               'lr-spec)))
        (each (source) ('("1 + 2 * (3 - 4)" "foo-7/2" "-x+3"))
           (scope ((tokens
                 (parser-significant-tokens
                  arithmetic-parser
                  (lex-source arithmetic-parser source))))
             (equal (equal? (parsed-language-root canonical tokens)
                            (parsed-language-root direct tokens))
                     #t)))))
    (scenario "official HCL corpus keeps LALR and direct parse products"
      (scope ((lalr (parser-ir-ref hcl-parser-ir 'lr-spec))
            (direct
             (parser-ir-ref
              (compile-parser hcl-grammar 'follow-partition-lr1)
              'lr-spec)))
        (equal (lr-spec-ref direct 'algorithm)  'follow-partition-lr1-v1)
        (equal (lr-spec-ref direct 'state-count)
                (lr-spec-ref lalr 'state-count))
        (each (fixture) ((language-loader-fixtures hcl-test-language 'accepted))
           (sequential ((source (syntax-fixture-source fixture))
                  (tokens
                   (parser-significant-tokens
                    hcl-parser
                    (lex-source hcl-parser source))))
             (equal (equal? (parsed-language-root lalr tokens)
                            (parsed-language-root direct tokens))
                     #t))))))
