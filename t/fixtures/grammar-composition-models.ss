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

;;; Both rules recognize the same spelling. Only the LR state can select the
;;; correct token identity for each position; global longest-match necessarily
;;; selects the declaration-first identity twice.
(begin
 (deflanguage directed-lexical-mode
  (syntax
   (lexical
    (root source-file)
    (lex
   (first FirstToken (literals "x" "y"))
   (second SecondToken (literals "x" "y")))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
      (repeat
       (seq (field first first) (field second second)))))))
 (bind-fixture-grammar-release directed-lexical-mode "directed-lexical-mode" "v1" "directed-lexical-mode.v1") )

;;; The mode-local regular DFA must admit whole rules by LR state. Both
;;; identifiers have equal source behavior but distinct token identities.
(begin
 (deflanguage directed-regular-mode
  (syntax
   (lexical
    (root source-file)
    (lex
   (first FirstToken (identifier))
   (second SecondToken (identifier))
   (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (node SourceFile
      (seq (field first first) (field second second))))))
 (bind-fixture-grammar-release directed-regular-mode "directed-regular-mode" "v1" "directed-regular-mode.v1") )

(defgrammar-role base-lexical-role
  (syntax-kinds
   (Punctuation token (text)))
  (terminals
   (punctuation Punctuation))
  (lexical-rules
   (punctuation (literals "+" "-" "*" "**" "/" "(" ")")))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar base-object-grammar
  (supers)
  (roles base-lexical-role))

(defgrammar-role appended-identifier-role
  (syntax-kinds (Identifier token (text)))
  (terminals (identifier Identifier))
  (lexical-rules (identifier (identifier)))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar-compose explicit-composed-grammar
  (supers base-object-grammar)
  (compose (append appended-identifier-role)))

(defgrammar-role conflicting-lexical-role
  (syntax-kinds
   (Punctuation token (text)))
  (terminals
   (punctuation Punctuation))
  (lexical-rules
   (punctuation (literals "+" "-" "*" "/" "(" ")")))
  (rules)
  (extras)
  (keywords)
  (parser-entrypoints)
  (recoveries)
  (flow))

(defgrammar conflicting-arithmetic-grammar
  (supers base-object-grammar)
  (roles conflicting-lexical-role))

(defgrammar-role unresolved-reference-role
  (syntax-kinds
   (SourceFile node ())
   (Unknown token (text)))
  (terminals
   (unknown Unknown))
  (lexical-rules
   (unknown (fallback)))
  (rules
   (source-file (reference missing-rule)))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar unresolved-reference-grammar
  (supers)
  (roles unresolved-reference-role))

(defgrammar-role invalid-terminal-kind-role
  (syntax-kinds
   (SourceFile node (value)))
  (terminals
   (value SourceFile))
  (lexical-rules
   (value (identifier)))
  (rules
   (source-file
    (alias SourceFile (field value (token value)))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar invalid-terminal-kind-grammar
  (supers)
  (roles invalid-terminal-kind-role))

(defgrammar-role missing-lexical-rule-role
  (syntax-kinds
   (SourceFile node (value))
   (Identifier token (text)))
  (terminals
   (identifier Identifier))
  (lexical-rules)
  (rules
   (source-file
    (alias SourceFile (field value (token identifier)))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar missing-lexical-rule-grammar
  (supers)
  (roles missing-lexical-rule-role))

(defgrammar-role unsupported-precedence-role
  (syntax-kinds
   (SourceFile node (value))
   (Identifier token (text)))
  (terminals
   (identifier Identifier))
  (lexical-rules
   (identifier (identifier)))
  (rules
   (source-file
    (alias SourceFile
      (field value
        (prec left 10 (token identifier))))))
  (extras)
  (keywords)
  (parser-entrypoints
   (source-file parse pure))
  (recoveries)
  (flow
   (source lexical)
   (lexical cst)))

(defgrammar unsupported-precedence-grammar
  (supers)
  (roles unsupported-precedence-role))

(def (condition-message thunk)
  (with-catch (lambda (condition) (error-message condition)) thunk))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (compile-composed grammar)
  (compile-parser (compile-grammar grammar)))

(def dynamic-precedence-rules
  '((source-file
     (precedence dynamic 1
      (alias SourceFile (field value (token identifier)))))))

(def competing-dynamic-precedence-rules
  '((source-file
     (choice (reference low-path) (reference high-path)))
    (low-path
     (precedence dynamic 1
      (alias LowPath (field value (token identifier)))))
    (high-path
     (precedence dynamic 2
      (alias HighPath (field value (token identifier)))))))

;;; The two reductions accept the same token and publish the same tree.  A
;;; correct selective-GLR runtime must execute both and merge their completed
;;; candidates even when neither production carries dynamic precedence.
(def static-equivalent-ambiguity-rules
  '((source-file
     (choice (reference first-path) (reference second-path)))
    (first-path
     (alias SourceFile (field value (token identifier))))
    (second-path
     (alias SourceFile (field value (token identifier))))))

;;; The same reduce/reduce shape deliberately publishes distinct roots.  The
;;; runtime must not turn declaration order into an implicit ambiguity policy.
(def static-distinct-ambiguity-rules
  '((source-file
     (choice (reference first-path) (reference second-path)))
    (first-path
     (alias FirstPath (field value (token identifier))))
    (second-path
     (alias SecondPath (field value (token identifier))))))

(def ambiguous-left-recursive-rules
  '((source-file
     (choice
      (sequence (reference source-file) (reference source-file))
      (alias SourceFile (field value (token identifier)))))))

(def selective-session-rules
  '((source-file
     (alias SourceFile
      (sequence (reference session-activity)
                (optional (reference session-close)))))
    (session-activity (repeat1 (reference session-set)))
    (session-set (sequence (literal "SESSION") (literal "SET")))
    (session-close (sequence (literal "SESSION") (literal "CLOSE")))))

(def nonassociative-rules
  '((source-file
     (alias SourceFile (field expression (reference expression))))
    (expression
     (choice
      (precedence none 10
       (alias ComparisonExpression
        (sequence
         (field left (reference expression))
         (field operator (literal "<"))
         (field right (reference expression)))))
      (alias NumberExpression (field value (token number)))))))

;;; Shared fixture declarations have a separate native compiler owner.
(export directed-lexical-mode-parser directed-regular-mode-parser
        base-lexical-role base-object-grammar appended-identifier-role
        explicit-composed-grammar conflicting-lexical-role
        conflicting-arithmetic-grammar unresolved-reference-role
        unresolved-reference-grammar invalid-terminal-kind-role
        invalid-terminal-kind-grammar missing-lexical-rule-role
        missing-lexical-rule-grammar unsupported-precedence-role
        unsupported-precedence-grammar condition-message row-ref compile-composed
        dynamic-precedence-rules competing-dynamic-precedence-rules
        static-equivalent-ambiguity-rules static-distinct-ambiguity-rules
        ambiguous-left-recursive-rules selective-session-rules nonassociative-rules)
