#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Grammar IR qualification of the optional Scheme LR construction.

(import :std/test
        (only-in :core/observability/testing-case poo-flow-test-case)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-grammar arithmetic-parser)
        (only-in :gerbil-parser/src/compiler/parser-ir
                 compile-parser parser-ir-ref)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-not-lalr-rules precedence-expression-rules))

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
      (error "arithmetic parser left a token suffix" rest))
    root))

(def (check-grammar-ir-equivalence rules sources)
  (let* ((grammar (grammar-ir rules))
         (canonical
          (parser-ir-ref (compile-parser grammar 'canonical-lr1) 'lr-spec))
         (direct
          (parser-ir-ref (compile-parser grammar 'follow-partition-lr1)
                         'lr-spec)))
    (check (lr-spec-ref direct 'algorithm) => 'follow-partition-lr1-v1)
    (check (> (lr-spec-ref direct 'state-count) 0) => #t)
    (for-each
     (lambda (source)
       (check (equal? (parsed-root canonical source)
                      (parsed-root direct source))
              => #t))
     sources)))

(def follow-partition-language-test
  (test-suite "follow partition through Grammar IR"
    (poo-flow-test-case "default Grammar IR construction remains LALR"
      (let (grammar (grammar-ir precedence-expression-rules))
        (check (parser-ir-ref (compile-parser grammar) 'lr-spec)
               => (parser-ir-ref (compile-parser grammar 'lalr) 'lr-spec))))
    (poo-flow-test-case "LR(1)-only grammar keeps its parse products"
      (check-grammar-ir-equivalence
       lr1-not-lalr-rules '("acd" "bce" "ace" "bcd")))
    (poo-flow-test-case "precedence grammar keeps its parse products"
      (check-grammar-ir-equivalence
       precedence-expression-rules '("a" "a+a" "a*a" "a+a*a" "a*a+a")))
    (poo-flow-test-case "generated arithmetic language keeps parse products"
      (let* ((canonical
              (parser-ir-ref
               (compile-parser arithmetic-grammar 'canonical-lr1)
               'lr-spec))
             (direct
              (parser-ir-ref
               (compile-parser arithmetic-grammar 'follow-partition-lr1)
               'lr-spec)))
        (for-each
         (lambda (source)
           (let (tokens
                 (parser-significant-tokens
                  arithmetic-parser
                  (lex-source arithmetic-parser source)))
             (check (equal? (parsed-language-root canonical tokens)
                            (parsed-language-root direct tokens))
                    => #t)))
         '("1 + 2 * (3 - 4)" "foo-7/2" "-x+3"))))))

(export follow-partition-language-test)
