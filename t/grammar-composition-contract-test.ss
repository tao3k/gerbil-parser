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

(def grammar-composition-contract-test
  (test-suite "grammar composition contracts"
    (test-case "POO contracts own grammar values and section dispatch"
      (check (grammar-role? base-lexical-role) => #t)
      (check (grammar? base-object-grammar) => #t)
      (check (grammar-role-name base-lexical-role) => 'base-lexical-role)
      (check (grammar-role-ref base-lexical-role 'terminals)
             => '((punctuation Punctuation)))
      (let (refined-role
            (.o (:: @ base-lexical-role)
                (.section
                 (lambda (field)
                   (if (eq? field 'terminals)
                     '((identifier Identifier))
                     (grammar-role-ref base-lexical-role field))))))
        (check (grammar-role-ref refined-role 'terminals)
               => '((identifier Identifier)))
        (check (grammar-role-ref base-lexical-role 'terminals)
               => '((punctuation Punctuation))))
      (check (row-ref (grammar->alist base-object-grammar) 'schema)
             => "gerbil-parser.grammar.v1")
      (check-exception
       (make-grammar 'projection-is-not-a-parent
                     (list arithmetic-grammar)
                     (list base-lexical-role))
       true))
    (test-case "normalization is deterministic"
      (check (grammar-ir-canonical (compile-grammar arithmetic-grammar))
             =>
             (grammar-ir-canonical (compile-grammar arithmetic-grammar))))
    (test-case "standard vector traversal interns projected catalogs"
      (let-values
          (((catalog canonical)
            (vector-intern-map
             '#(left right left right)
             (lambda (value) value)
             (lambda (key id) (cons id key)))))
        (check (vector-length canonical) => 2)
        (check (eq? (vector-ref catalog 0) (vector-ref catalog 2)) => #t)
        (check (eq? (vector-ref catalog 1) (vector-ref catalog 3)) => #t)))
    (test-case "LR states direct identical lexical spellings"
      (let ((global-tokens (lex-source directed-lexical-mode-parser "xx"))
            (artifact (parse-source directed-lexical-mode-parser "xx")))
        (check (map token-kind global-tokens) => '(first first))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => "xx")))
    (test-case "mode-local regular DFA keeps LR token identities"
      (let ((global-tokens (lex-source directed-regular-mode-parser "x x"))
            (artifact (parse-source directed-regular-mode-parser "x x")))
        (check (map token-kind global-tokens) => '(first space first))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => "x x")))
    (test-case "character certificates veto competing token identities"
      (for-each
       (lambda (machine)
         (let* ((catalog (lr-runtime-lexical-mode-catalog (parser-machine-runtime machine)))
                (lexer (parser-machine-lex machine)) (competing 0))
           (for-each
            (lambda (i)
              (for-each
               (lambda (j)
                 (let-values (((left left-end) (lexer "x" 0 0 (vector-ref catalog i)))
                              ((right right-end) (lexer "x" 0 0 (vector-ref catalog j))))
                   (unless (eq? (token-kind left) (token-kind right))
                     (set! competing (+ competing 1))
                     (check (parser-machine-lexical-modes-compatible? machine i j #\x) => #f))))
               (iota (vector-length catalog))))
            (iota (vector-length catalog)))
           (check (> competing 0) => #t)))
       (list directed-lexical-mode-parser directed-regular-mode-parser)))
    (test-case "incremental reuse honors LR lexical modes on identical spellings"
      (let* ((source (make-string 96 #\x))
             (source-edit (make-edit 48 1 "y"))
             (session
              (make-incremental-session
               directed-lexical-mode-parser source))
             (fresh
              (parse-source directed-lexical-mode-parser
                            (apply-edit source source-edit))))
        (let-values (((next receipt)
                      (parse-incremental-session session source-edit))
                     ((one-shot one-shot-receipt)
                      (parse-source/incremental
                       directed-lexical-mode-parser source
                       (parse-source directed-lexical-mode-parser source)
                       source-edit)))
          (check (incremental-session-artifact next) => fresh)
          (check one-shot => fresh)
          (check (or (> (or (row-ref receipt 'checkpointReusedShiftCount)
                            0) 0)
                     (> (or (row-ref receipt 'reusedRecognitionEventCount)
                            0) 0))
                 => #t)
          (check (> (row-ref receipt 'reusedSuffixTokenCount) 0) => #t)
          (check (row-ref one-shot-receipt 'freshFallback?) => #f))))
    (test-case "explicit POO composition emits an identity-bearing receipt"
      (let-values (((ir receipt)
                    (compile-grammar/receipt explicit-composed-grammar)))
        (check (grammar-ir-ref ir 'terminals)
               => '((punctuation Punctuation) (identifier Identifier)))
        (check (row-ref receipt 'schema)
               => "gerbil-parser.grammar-composition-receipt.v1")
        (check (grammar-ir-ref ir 'compositionDigest)
               => (row-ref receipt 'compositionDigest))
        (check (map (lambda (step) (row-ref step 'operation))
                    (row-ref receipt 'steps))
               => '(merge append))))
    (test-case "explicit append fails closed on an existing identity"
      (let (conflicting
            (make-grammar
             'conflicting-explicit-append
             (list base-object-grammar)
             '()
             (list (cons 'append base-lexical-role))))
        (check (condition-message (lambda () (compile-grammar conflicting)))
               => "grammar append target already exists")))
))
(export grammar-composition-contract-test)
