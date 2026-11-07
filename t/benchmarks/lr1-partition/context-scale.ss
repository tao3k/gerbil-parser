#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Complete Scheme LR construction as independent context count grows.

(import (only-in :gerbil-parser/src/compiler/funcs
                 compiler-index-set-for-each)
        (only-in :gerbil-parser/src/compiler/lr
                 compute-first compute-nullable lower-rules lr-spec-ref
                 production-table)
        (only-in :gerbil-parser/src/compiler/lr-conflict-candidates
                 forward-reachable-follow-masks raw-conflict-cells
                 initial-backward-follow-partitions)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr-lookahead
                 build-states-via-lr0)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-context-family-rules))

(def (measure size rules construction sample)
  (let* ((started (##current-time-point))
         (spec (compile-lr-spec rules 'source-file 'reject #f construction))
         (elapsed-ms (* 1000.0 (- (##current-time-point) started))))
    (list (cons 'workload 'grammar-construction)
          (cons 'grammar-contexts size)
          (cons 'construction construction)
          (cons 'sample sample)
          (cons 'elapsed-ms elapsed-ms)
          (cons 'state-count (lr-spec-ref spec 'state-count))
          (cons 'follow-block-count
                (or (lr-spec-ref spec 'follow-block-count) 0))
          (cons 'construction-items
                (or (lr-spec-ref spec 'output-item-count)
                    (lr-spec-ref spec 'lookahead-item-visit-count))))))

(def (measure-seed size rules)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let ((started (##current-time-point))
              (production-follow-pairs 0)
              (nfa-vertices 0)
              (lr0-states 0)
              (conflict-states 0))
          (let-values (((states count lookaheads offsets transitions
                                terminal-values layout core-symbols
                                state-visits item-visits)
                        (build-states-via-lr0
                         productions table first-index nullable-index)))
            (set! lr0-states count)
            (let (candidates
                  (raw-conflict-cells
                   states count lookaheads offsets terminal-values
                   layout core-symbols))
              (for-each
               (lambda (mask)
                 (unless (zero? mask)
                   (set! conflict-states (+ conflict-states 1))))
               (vector->list candidates))))
          (let-values (((follows terminals)
                        (forward-reachable-follow-masks
                         productions table first-index nullable-index)))
            (for-each
             (lambda (mask)
               (compiler-index-set-for-each
                mask (lambda (terminal)
                       (set! production-follow-pairs
                             (+ production-follow-pairs 1)))))
             (vector->list follows)))
          (let-values (((partitions candidates terminals)
                        (initial-backward-follow-partitions
                         productions table first-index nullable-index)))
            (for-each
             (lambda (blocks)
               (when blocks
                 (for-each
                  (lambda (block)
                    (compiler-index-set-for-each
                     (cdr block)
                     (lambda (terminal)
                       (set! nfa-vertices (+ nfa-vertices 1)))))
                  blocks)))
             (vector->list partitions))
            (list (cons 'workload 'grammar-construction)
                  (cons 'grammar-contexts size)
                  (cons 'construction 'initial-backward-seed)
                  (cons 'elapsed-ms
                        (* 1000.0 (- (##current-time-point) started)))
                  (cons 'production-follow-pairs production-follow-pairs)
                  (cons 'productions (vector-length table))
                  (cons 'lr0-states lr0-states)
                  (cons 'conflict-states conflict-states)
                  (cons 'nfa-vertices nfa-vertices)
                  (cons 'initial-blocks
                        (foldl (lambda (blocks count)
                                 (+ count (if blocks (length blocks) 0)))
                               0 (vector->list partitions))))))))))

(def (main . args)
  (let ((size (if (pair? args) (string->number (car args)) 64))
        (samples (if (and (pair? args) (pair? (cdr args)))
                   (string->number (cadr args)) 3)))
    (unless (and (integer? size) (> size 0)
                 (integer? samples) (> samples 0))
      (error "expected positive context count and sample count" args))
    (let (rules (lr1-context-family-rules size))
      (write (measure-seed size rules))
      (newline)
      (for-each
       (lambda (construction)
         (let loop ((sample 1))
           (when (<= sample samples)
             (write (measure size rules construction sample))
             (newline)
             (loop (+ sample 1)))))
       '(canonical-lr1 follow-partition-lr1)))))

(export main)
