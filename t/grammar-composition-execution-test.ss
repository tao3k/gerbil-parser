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

(def grammar-composition-execution-test
  (test-suite "grammar composition execution"
    (test-case "selective GLR commits the only successful session branch"
      (let (spec
            (compile-lr-spec selective-session-rules
                             'source-file 'selective-glr))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "CLOSE" 20 25)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'punctuation "SESSION" 0 7)
                             (make-token 'punctuation "SET" 8 11)
                             (make-token 'punctuation "SESSION" 12 19)
                             (make-token 'punctuation "SET" 20 23)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))))
    (test-case "static selective GLR merges equivalent completed branches"
      (let (spec
            (compile-lr-spec static-equivalent-ambiguity-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '())
          (check (row-ref receipt 'branchesExplored) => 2)
          (check (row-ref receipt 'mergedBranches) => 1)
          (check (row-ref receipt 'ambiguousBranches) => 0))))
    (test-case "static selective GLR rejects distinct completed branches"
      (let (spec
            (compile-lr-spec static-distinct-ambiguity-rules
                             'source-file 'selective-glr))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (call-with-values
             (lambda ()
               (lr-parse/receipt
                spec (list (make-token 'identifier "x" 0 1))))
             (lambda _ #f))))
         => "selective GLR ambiguity is unresolved")))
    (test-case "dynamic precedence requires selective GLR admission"
      (check (condition-message
              (lambda ()
                (compile-lr-spec dynamic-precedence-rules 'source-file)))
             => "dynamic precedence requires selective GLR admission")
      (let (spec
            (compile-lr-spec dynamic-precedence-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '())
          (check (row-ref receipt 'dynamicScore) => 1)
          (check (row-ref receipt 'schema)
                 => "gerbil-parser.selective-glr-receipt.v1"))))
    (test-case "dynamic precedence ranks completed reduce branches"
      (check
       (condition-message
        (lambda ()
          (compile-lr-spec competing-dynamic-precedence-rules
                           'source-file)))
       => "dynamic precedence requires selective GLR admission")
      (let (spec
            (compile-lr-spec competing-dynamic-precedence-rules
                             'source-file 'selective-glr))
        (let-values (((root rest receipt)
                      (lr-parse/receipt
                       spec (list (make-token 'identifier "x" 0 1)))))
          (check (recognition-node-kind root) => 'HighPath)
          (check rest => '())
          (check (row-ref receipt 'branchesExplored) => 2)
          (check (row-ref receipt 'successfulCompletions) => 2)
          (check (row-ref receipt 'distinctCompletions) => 2)
          (check (row-ref receipt 'winnerReason) => 'dynamic-precedence)
          (check (row-ref receipt 'dynamicScore) => 2))))
    (test-case "immutable checkpoints resume the sole LR executor"
      (let* ((spec (compile-lr-spec nonassociative-rules 'source-file))
             (runtime (lr-prepare spec))
             (tokens
              (list (make-token 'number "1" 0 1)
                    (make-token 'punctuation "<" 1 2)
                    (make-token 'number "2" 2 3)))
             (initial (lr-initial-checkpoint runtime tokens)))
        (check (lr-checkpoint? initial) => #t)
        (check (lr-checkpoint-deterministic-actions initial) => 0)
        (check (lr-checkpoint-remaining-token-count initial) => 3)
        (let-values (((status checkpoint) (lr-checkpoint-advance initial 1)))
          (check status => 'checkpoint)
          (check (lr-checkpoint-deterministic-actions checkpoint) => 1)
          (check (lr-checkpoint-deterministic-shifts checkpoint) => 1)
          (check (lr-checkpoint-remaining-token-count checkpoint) => 2)
          (let-values (((resumed-root resumed-rest)
                        (lr-checkpoint-resume checkpoint))
                       ((fresh-root fresh-rest) (lr-parse spec tokens)))
            (check resumed-root => fresh-root)
            (check resumed-rest => fresh-rest)))))
    (test-case "non-associative precedence rejects only chained operators"
      (let (spec (compile-lr-spec nonassociative-rules 'source-file))
        (let-values (((root rest)
                      (lr-parse
                       spec
                       (list (make-token 'number "1" 0 1)
                             (make-token 'punctuation "<" 1 2)
                             (make-token 'number "2" 2 3)))))
          (check (recognition-node-kind root) => 'SourceFile)
          (check rest => '()))
        (check-exception
         (lr-parse
          spec
          (list (make-token 'number "1" 0 1)
                (make-token 'punctuation "<" 1 2)
                (make-token 'number "2" 2 3)
                (make-token 'punctuation "<" 3 4)
                (make-token 'number "3" 4 5)))
         true)))
    (test-case "same lexical identity with a different definition is rejected"
      (check-exception
       (compile-grammar conflicting-arithmetic-grammar)
       true))
    (test-case "the generic machine executes the declared entrypoint"
      (let-values (((root rest)
                    ((parser-machine-parse arithmetic-parser)
                     (list (make-token 'number "1" 0 1)))))
        (check (recognition-node-kind root) => 'SourceFile)
        (check rest => '())))
    (test-case "extras generate the sole trivia predicate"
      (check ((parser-machine-trivia arithmetic-parser)
              (make-token 'whitespace " " 0 1))
             => '(whitespace))
      (check ((parser-machine-trivia arithmetic-parser)
              (make-token 'punctuation "+" 0 1))
             => #f))))
(export grammar-composition-execution-test)
