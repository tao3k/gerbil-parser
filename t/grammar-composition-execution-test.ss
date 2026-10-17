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

(deflanguage-parser-tests grammar-composition-execution-test "grammar composition execution"
  (engine)
    (scenario "selective GLR commits the only successful session branch"
      (scope ((spec
            (compile-lr-spec selective-session-rules
                             'source-file 'selective-glr)))
        (scope-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "CLOSE" 20 25)))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '()))
        (scope-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "SET" 20 23)))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '()))))
    (scenario "static selective GLR merges equivalent completed branches"
      (scope ((spec
            (compile-lr-spec static-equivalent-ambiguity-rules
                             'source-file 'selective-glr)))
        (scope-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '())
          (equal (row-ref receipt 'branchesExplored)  2)
          (equal (row-ref receipt 'mergedBranches)  1)
          (equal (row-ref receipt 'ambiguousBranches)  0))))
    (property "static selective GLR rejects distinct completed branches"
      (bindings (spec
            (compile-lr-spec static-distinct-ambiguity-rules
                             'source-file 'selective-glr)))
      (equal
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (call-with-values
             (lambda ()
               (lr-parse/receipt
                spec (list (make-token 'identifier "x" 0 1))))
             (lambda _ #f))))
          "selective GLR ambiguity is unresolved"))
    (scenario "dynamic precedence requires selective GLR admission"
      (equal (condition-message
              (lambda ()
                (compile-lr-spec dynamic-precedence-rules 'source-file)))
              "dynamic precedence requires selective GLR admission")
      (scope ((spec
            (compile-lr-spec dynamic-precedence-rules
                             'source-file 'selective-glr)))
        (scope-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '())
          (equal (row-ref receipt 'dynamicScore)  1)
          (equal (row-ref receipt 'schema)
                  "gerbil-parser.selective-glr-receipt.v1"))))
    (scenario "dynamic precedence ranks completed reduce branches"
      (equal
       (condition-message
        (lambda ()
          (compile-lr-spec competing-dynamic-precedence-rules
                           'source-file)))
        "dynamic precedence requires selective GLR admission")
      (scope ((spec
            (compile-lr-spec competing-dynamic-precedence-rules
                             'source-file 'selective-glr)))
        (scope-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (equal (recognition-node-kind root)  'HighPath)
          (equal rest  '())
          (equal (row-ref receipt 'branchesExplored)  2)
          (equal (row-ref receipt 'successfulCompletions)  2)
          (equal (row-ref receipt 'distinctCompletions)  2)
          (equal (row-ref receipt 'winnerReason)  'dynamic-precedence)
          (equal (row-ref receipt 'dynamicScore)  2))))
    (scenario "immutable checkpoints resume the sole LR executor"
      (sequential ((spec (compile-lr-spec nonassociative-rules 'source-file))
             (runtime (lr-prepare spec))
             (tokens
              (list (make-token 'number "1" 0 1)
                    (make-token 'punctuation "<" 1 2)
                    (make-token 'number "2" 2 3)))
             (initial (lr-initial-checkpoint runtime tokens)))
        (equal (lr-checkpoint? initial)  #t)
        (equal (lr-checkpoint-deterministic-actions initial)  0)
        (equal (lr-checkpoint-remaining-token-count initial)  3)
        (scope-values (((status checkpoint) (lr-checkpoint-advance initial 1)))
          (equal status  'checkpoint)
          (equal (lr-checkpoint-deterministic-actions checkpoint)  1)
          (equal (lr-checkpoint-deterministic-shifts checkpoint)  1)
          (equal (lr-checkpoint-remaining-token-count checkpoint)  2)
          (scope-values (((resumed-root resumed-rest)
                        (lr-checkpoint-resume checkpoint))
                       ((fresh-root fresh-rest) (lr-parse spec tokens)))
            (equal resumed-root  fresh-root)
            (equal resumed-rest  fresh-rest)))))
    (scenario "non-associative precedence rejects only chained operators"
      (scope ((spec (compile-lr-spec nonassociative-rules 'source-file)))
        (scope-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'number "1" 0 1)
                             (make-token 'punctuation "<" 1 2)
                             (make-token 'number "2" 2 3)))))
          (equal (recognition-node-kind root)  'SourceFile)
          (equal rest  '()))
        (raises
         (lr-parse
          spec
          (list (make-token 'number "1" 0 1)
                (make-token 'punctuation "<" 1 2)
                (make-token 'number "2" 2 3)
                (make-token 'punctuation "<" 3 4)
                (make-token 'number "3" 4 5)))
         true)))
    (scenario "same lexical identity with a different definition is rejected"
      (raises
       (compile-grammar conflicting-arithmetic-grammar)
       true))
    (scenario "the generic machine executes the declared entrypoint"
      (scope-values (((root rest)
                    ((parser-machine-parse arithmetic-parser)
                     (list (make-token 'number "1" 0 1)))))
        (equal (recognition-node-kind root)  'SourceFile)
        (equal rest  '())))
    (property "extras generate the sole trivia predicate"
      (bindings)
      (equal ((parser-machine-trivia arithmetic-parser)
              (make-token 'whitespace " " 0 1))
              '(whitespace))
      (equal ((parser-machine-trivia arithmetic-parser)
              (make-token 'punctuation "+" 0 1))
              #f)))
