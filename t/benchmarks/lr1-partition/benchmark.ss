#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Isolated Scheme parser-generator construction receipt.

(import (only-in :gerbil-parser/src/compiler/lr
                 compute-first compute-nullable lower-rules lr-spec-ref
                 production-table)
        (only-in :gerbil-parser/src/compiler/lr-conflict-candidates
                 initial-backward-follow-partitions
                 lr0-conflict-candidates)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-not-lalr-rules shared-lookahead-rules
                 precedence-expression-rules mixed-context-rules
                 lr1-context-family-rules))

(def (timing-fields started repetitions)
  (let (total-ms (* 1000.0 (- (##current-time-point) started)))
    (list (cons 'elapsed-total-ms total-ms)
          (cons 'mean-us (/ (* 1000.0 total-ms) repetitions)))))

(def (chain-rules count)
  (def (rule-name index)
    (string->symbol (string-append "chain-" (number->string index))))
  (cons
   (list 'source-file
         (list 'alias 'SourceFile (list 'reference (rule-name 0))))
   (map
    (lambda (index)
      (list (rule-name index)
            (if (= index (- count 1))
              (list 'literal (number->string index))
              (list 'sequence
                    (list 'literal (number->string index))
                    (list 'reference (rule-name (+ index 1)))))))
    (iota count))))

(def (measure grammar rules construction repetitions)
  (let ((started (##current-time-point))
        (last #f))
    (let loop ((remaining repetitions))
      (unless (zero? remaining)
        (set! last
              (compile-lr-spec rules 'source-file 'reject #f construction))
        (loop (- remaining 1))))
    (let (timing (timing-fields started repetitions))
      (list (cons 'grammar grammar)
            (cons 'construction construction)
            (cons 'repetitions repetitions)
            (cons 'state-count (lr-spec-ref last 'state-count))
            (cons 'construction-items
                  (or (lr-spec-ref last 'output-item-count)
                      (lr-spec-ref last 'lookahead-item-visit-count)))
            (car timing) (cadr timing)))))

(def (character-tokens source)
  (let loop ((index 0) (tokens '()))
    (if (= index (string-length source))
      (reverse tokens)
      (loop (+ index 1)
            (cons (make-token 'punctuation
                              (string (string-ref source index))
                              index (+ index 1))
                  tokens)))))

;; The table and token stream are prepared outside the timed loop. This
;; measures fresh LR parsing, not grammar construction or lexing.
(def (measure-parse grammar rules construction source repetitions)
  (let* ((spec (compile-lr-spec rules 'source-file 'reject #f construction))
         (tokens (character-tokens source))
         (last #f))
    (def (run-batch count)
      (let (started (##current-time-point))
        (let loop ((remaining count))
          (unless (zero? remaining)
            (let-values (((root rest) (lr-parse spec tokens)))
              (unless (null? rest)
                (error "parser left a token suffix" grammar source rest))
              (set! last root))
            (loop (- remaining 1))))
        (/ (* 1000000.0 (- (##current-time-point) started)) count)))
    (run-batch (min 100 repetitions))
    (let* ((first (run-batch repetitions))
           (second (run-batch repetitions))
           (third (run-batch repetitions))
           (minimum (min first second third))
           (maximum (max first second third)))
      (unless last (error "parser produced no root" grammar source))
      (list (cons 'workload 'fresh-lr-parse)
            (cons 'grammar grammar)
            (cons 'construction construction)
            (cons 'source source)
            (cons 'repetitions repetitions)
            (cons 'batches 3)
            (cons 'median-us (- (+ first second third) minimum maximum))
            (cons 'min-us minimum)
            (cons 'max-us maximum)))))

;; Phase-only timing: the lowered grammar and FIRST/nullability are fixed.
;; Compare this with itself across changes, not with full compile-lr-spec time.
(def (measure-direct-seed grammar rules repetitions)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let ((started (##current-time-point))
              (last #f))
          (let loop ((remaining repetitions))
            (unless (zero? remaining)
              (let-values (((partitions candidates terminals)
                            (initial-backward-follow-partitions
                             productions table first-index nullable-index)))
                (set! last partitions))
              (loop (- remaining 1))))
          (let (timing (timing-fields started repetitions))
            (list (cons 'grammar grammar)
                  (cons 'construction 'lr0-initial-backward-seed)
                  (cons 'repetitions repetitions)
                  (cons 'block-count
                        (foldl (lambda (blocks count)
                                 (+ count (if blocks (length blocks) 0)))
                               0 (vector->list last)))
                  (car timing) (cadr timing))))))))

;; Isolate LR(0) construction plus raw candidate detection from the follow
;; partition refinements. The candidate count is a semantic receipt alongside
;; the timing, so an apparent speedup cannot silently drop conflict cells.
(def (measure-candidates grammar rules repetitions)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let ((started (##current-time-point))
              (last #f))
          (let loop ((remaining repetitions))
            (unless (zero? remaining)
              (let-values (((states count terminals candidates)
                            (lr0-conflict-candidates
                             productions table first-index nullable-index)))
                (set! last candidates))
              (loop (- remaining 1))))
          (let (timing (timing-fields started repetitions))
            (list (cons 'grammar grammar)
                  (cons 'construction 'lr0-conflict-candidates)
                  (cons 'repetitions repetitions)
                  (cons 'conflict-state-count
                        (foldl (lambda (mask count)
                                 (+ count (if (zero? mask) 0 1)))
                               0 (vector->list last)))
                  (car timing) (cadr timing))))))))

(def (main . args)
  (let (repetitions
        (if (null? args) 100 (string->number (car args))))
    (unless (and (integer? repetitions) (> repetitions 0))
      (error "expected a positive repetition count" args))
    (for-each
     (lambda (entry)
       (write (measure (car entry) (cadr entry) (caddr entry)
                       repetitions))
       (newline))
     (list
      (list 'shared-lookahead shared-lookahead-rules 'lalr)
      (list 'shared-lookahead shared-lookahead-rules 'lalr-then-follow)
      (list 'shared-lookahead shared-lookahead-rules 'canonical-lr1)
      (list 'shared-lookahead shared-lookahead-rules 'partitioned-lr1)
      (list 'shared-lookahead shared-lookahead-rules 'follow-partition-lr1)
      (list 'lr1-not-lalr lr1-not-lalr-rules 'canonical-lr1)
      (list 'lr1-not-lalr lr1-not-lalr-rules 'lalr-then-follow)
      (list 'lr1-not-lalr lr1-not-lalr-rules 'partitioned-lr1)
      (list 'lr1-not-lalr lr1-not-lalr-rules 'follow-partition-lr1)
      (list 'precedence precedence-expression-rules 'canonical-lr1)
      (list 'precedence precedence-expression-rules
            'follow-partition-lr1)
      (list 'mixed-context mixed-context-rules 'canonical-lr1)
      (list 'mixed-context mixed-context-rules 'follow-partition-lr1)))
    (for-each
     (lambda (entry)
       (write (measure-direct-seed (car entry) (cadr entry) repetitions))
       (newline))
     (list (list 'shared-lookahead shared-lookahead-rules)
           (list 'lr1-not-lalr lr1-not-lalr-rules)))
    (for-each
     (lambda (entry)
       (write (measure-candidates (car entry) (cadr entry) repetitions))
       (newline))
     (list (list 'lr1-not-lalr lr1-not-lalr-rules)
           (list 'mixed-context mixed-context-rules)))
    (let ((large (chain-rules 64))
          (large-repetitions (max 1 (quotient repetitions 10))))
      (for-each
       (lambda (construction)
         (write (measure 'chain-64 large construction
                         large-repetitions))
         (newline))
       '(lalr canonical-lr1 follow-partition-lr1))
      (write (measure-direct-seed 'chain-64 large large-repetitions))
      (newline)
      (write (measure-candidates 'chain-64 large large-repetitions))
      (newline))
    (for-each
     (lambda (size)
       (let ((rules (lr1-context-family-rules size))
             (family-repetitions (max 1 (quotient repetitions 10))))
         (for-each
          (lambda (construction)
            (write (measure
                    (list 'lr1-context-family size) rules construction
                    family-repetitions))
            (newline))
          '(canonical-lr1 follow-partition-lr1))))
     '(4 16))
    (let (parse-repetitions (* repetitions 10))
      (for-each
       (lambda (entry)
         (write (measure-parse (car entry) (cadr entry) (caddr entry)
                               (cadddr entry) parse-repetitions))
         (newline))
       (list
        (list 'shared-lookahead shared-lookahead-rules 'lalr "cdd")
        (list 'shared-lookahead shared-lookahead-rules
              'follow-partition-lr1 "cdd")
        (list 'lr1-not-lalr lr1-not-lalr-rules 'canonical-lr1 "acd")
        (list 'lr1-not-lalr lr1-not-lalr-rules
              'follow-partition-lr1 "acd")
        (list 'precedence precedence-expression-rules
              'canonical-lr1 "a+a*a")
        (list 'precedence precedence-expression-rules
              'follow-partition-lr1 "a+a*a"))))))

(export main)
