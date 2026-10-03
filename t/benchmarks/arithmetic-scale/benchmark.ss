#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Pure Scheme end-to-end parser scaling benchmark.

(import (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest parser-machine-runtime
                 parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact
                 parse-artifact-success? parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens))

(def (addition-source terms)
  (let (source (make-string (- (* 2 terms) 1) #\1))
    (let loop ((offset 1))
      (when (< offset (string-length source))
        (string-set! source offset #\+)
        (loop (+ offset 2))))
    source))

(def (measure terms)
  (let* ((source (addition-source terms))
         (samples
          (map (lambda (_)
                 (let* ((started (##current-time-point))
                        (artifact (parse-arithmetic-v1 source))
                        (elapsed-ms
                         (* 1000.0 (- (##current-time-point) started))))
                   (unless (and (parse-artifact-success? artifact)
                                (string=? source
                                          (parse-artifact-roundtrip artifact)))
                     (error "arithmetic scaling fixture did not roundtrip"
                            terms))
                   elapsed-ms))
               (iota 5))))
    (write (list (cons 'terms terms)
                 (cons 'bytes (string-length source))
                 (list 'samples-ms samples)
                 (cons 'best-ms (apply min samples))))
    (newline)))

(def (measure-phase name thunk)
  (let (samples
        (map (lambda (_)
               (##gc)
               (let* ((started (##current-time-point))
                      (cpu-started (cpu-time))
                      (result (thunk))
                      (cpu-ms (* 1000.0 (- (cpu-time) cpu-started)))
                      (elapsed-ms
                       (* 1000.0 (- (##current-time-point) started))))
                 (unless result
                   (error "phase returned no result" name))
                 (cons elapsed-ms cpu-ms)))
             (iota 5)))
    (list name
          (cons 'best-elapsed-ms (apply min (map car samples)))
          (cons 'best-cpu-ms (apply min (map cdr samples)))
          (cons 'samples-ms samples))))

(def (measure-stages terms)
  ;; Prepared tokens make these phase costs comparable across revisions, but
  ;; they are not a decomposition of the directed streaming parser path.
  (let* ((source (addition-source terms))
         (tokens (lex-source arithmetic-parser source))
         (significant (parser-significant-tokens arithmetic-parser tokens))
         (runtime (parser-machine-runtime arithmetic-parser)))
    (let-values (((root rest) (lr-parse/prepared runtime significant)))
      (unless (null? rest)
        (error "prepared LR left unconsumed tokens" terms))
      (unless
          (equal?
           (parse-arithmetic-v1 source)
           (make-success-parse-artifact
            (parser-machine-grammar-digest arithmetic-parser)
            source tokens root (parser-machine-trivia arithmetic-parser)))
        (error "directed and prepared parsers disagree" terms))
      (write
       (list (cons 'terms terms)
             (measure-phase 'lexical-analysis
                            (lambda () (lex-source arithmetic-parser source)))
             (measure-phase
              'significant-token-filter
              (lambda ()
                (parser-significant-tokens arithmetic-parser tokens)))
             (measure-phase
              'lr-execution
              (lambda ()
                (let-values
                    (((parsed remaining)
                      (lr-parse/prepared runtime significant)))
                  (unless (null? remaining)
                    (error "prepared LR left unconsumed tokens" terms))
                  parsed)))
             (measure-phase
              'artifact-materialization
              (lambda ()
                (make-success-parse-artifact
                 (parser-machine-grammar-digest arithmetic-parser)
                 source tokens root
                 (parser-machine-trivia arithmetic-parser))))))
      (newline))))

(def (main . args)
  (parse-arithmetic-v1 "1+1")
  (for-each measure
            (if (null? args)
              '(100 200 400 800 1600)
              (map string->number args)))
  (for-each measure-stages
            (if (null? args)
              '(1600)
              (map string->number args))))

(export main)
