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

(deflanguage-parser-tests grammar-composition-contract-test "grammar composition contracts"
  (engine)
    (scenario "POO contracts own grammar values and section dispatch"
      (equal (grammar-role? base-lexical-role)  #t)
      (equal (grammar? base-object-grammar)  #t)
      (equal (grammar-role-name base-lexical-role)  'base-lexical-role)
      (equal (grammar-role-ref base-lexical-role 'terminals)
              '((punctuation Punctuation)))
      (scope ((refined-role
            (.o (:: @ base-lexical-role)
                (.section
                 (lambda (field)
                   (if (eq? field 'terminals)
                     '((identifier Identifier))
                     (grammar-role-ref base-lexical-role field)))))))
        (equal (grammar-role-ref refined-role 'terminals)
                '((identifier Identifier)))
        (equal (grammar-role-ref base-lexical-role 'terminals)
                '((punctuation Punctuation))))
      (equal (row-ref (grammar->alist base-object-grammar) 'schema)
              "gerbil-parser.grammar.v1")
      (raises
       (make-grammar 'projection-is-not-a-parent
                     (list arithmetic-grammar)
                     (list base-lexical-role))
       true))
    (property "normalization is deterministic"
      (bindings)
      (equal (grammar-ir-canonical (compile-grammar arithmetic-grammar))

             (grammar-ir-canonical (compile-grammar arithmetic-grammar))))
    (scenario "standard vector traversal interns projected catalogs"
      (scope-values
          (((catalog canonical)
            (vector-intern-map
             '#(left right left right)
             (lambda (value) value)
             (lambda (key id) (cons id key)))))
        (equal (vector-length canonical)  2)
        (equal (eq? (vector-ref catalog 0) (vector-ref catalog 2))  #t)
        (equal (eq? (vector-ref catalog 1) (vector-ref catalog 3))  #t)))
    (scenario "LR states direct identical lexical spellings"
      (scope ((global-tokens (lex-source directed-lexical-mode-parser "xx"))
            (artifact (parse-source directed-lexical-mode-parser "xx")))
        (equal (map token-kind global-tokens)  '(first first))
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  "xx")))
    (scenario "mode-local regular DFA keeps LR token identities"
      (scope ((global-tokens (lex-source directed-regular-mode-parser "x x"))
            (artifact (parse-source directed-regular-mode-parser "x x")))
        (equal (map token-kind global-tokens)  '(first space first))
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  "x x")))
    (scenario "character certificates veto competing token identities"
      (each (machine) ((list directed-lexical-mode-parser directed-regular-mode-parser))
         (sequential ((catalog (lr-runtime-lexical-mode-catalog (parser-machine-runtime machine)))
                (lexer (parser-machine-lex machine)) (competing 0))
           (each (i) ((iota (vector-length catalog)))
              (each (j) ((iota (vector-length catalog)))
                 (scope-values (((left left-end) (lexer "x" 0 0 (vector-ref catalog i)))
                              ((right right-end) (lexer "x" 0 0 (vector-ref catalog j))))
                   (unless (eq? (token-kind left) (token-kind right))
                     (act (set! competing (+ competing 1)))
                     (equal (parser-machine-lexical-modes-compatible? machine i j #\x)  #f)))))
           (equal (> competing 0)  #t))))
    (scenario "incremental reuse honors LR lexical modes on identical spellings"
      (sequential ((source (make-string 96 #\x))
             (source-edit (make-edit 48 1 "y"))
             (session
              (make-incremental-session
               directed-lexical-mode-parser source))
             (fresh
              (parse-source directed-lexical-mode-parser
                            (apply-edit source source-edit))))
        (scope-values (((next receipt)
                      (parse-incremental-session session source-edit))
                     ((one-shot one-shot-receipt)
                      (parse-source/incremental
                       directed-lexical-mode-parser source
                       (parse-source directed-lexical-mode-parser source)
                       source-edit)))
          (equal (incremental-session-artifact next)  fresh)
          (equal one-shot  fresh)
          (equal (or (> (or (row-ref receipt 'checkpointReusedShiftCount)
                            0) 0)
                     (> (or (row-ref receipt 'reusedRecognitionEventCount)
                            0) 0))
                  #t)
          (equal (> (row-ref receipt 'reusedSuffixTokenCount) 0)  #t)
          (equal (row-ref one-shot-receipt 'freshFallback?)  #f))))
    (scenario "explicit POO composition emits an identity-bearing receipt"
      (scope-values (((ir receipt)
                    (compile-grammar/receipt explicit-composed-grammar)))
        (equal (grammar-ir-ref ir 'terminals)
                '((punctuation Punctuation) (identifier Identifier)))
        (equal (row-ref receipt 'schema)
                "gerbil-parser.grammar-composition-receipt.v1")
        (equal (grammar-ir-ref ir 'compositionDigest)
                (row-ref receipt 'compositionDigest))
        (equal (map (lambda (step) (row-ref step 'operation))
                    (row-ref receipt 'steps))
                '(merge append))))
    (property "explicit append fails closed on an existing identity"
      (bindings (conflicting
            (make-grammar
             'conflicting-explicit-append
             (list base-object-grammar)
             '()
             (list (cons 'append base-lexical-role)))))
      (equal (condition-message (lambda () (compile-grammar conflicting)))
                "grammar append target already exists"))
)
