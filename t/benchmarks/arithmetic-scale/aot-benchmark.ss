#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Measure a preloaded arithmetic source in an optimized native executable.
;;; Recognition modes stop before ParseArtifact event materialization. They use
;;; the full-source lexer; the preflight compares its product with directed lexing.

(import (only-in :gerbil-parser/languages/arithmetic/v1/parser
                 arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest parser-machine-runtime
                 parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-success?
                 parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse/prepared)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens))

(def (addition-source terms shape)
  (let* ((width (if (eq? shape 'lines) 3 2))
         (source (make-string (- (* width terms) (fx- width 1)) #\1)))
    (let loop ((offset 1))
      (when (< offset (string-length source))
        (string-set! source offset #\+)
        (when (eq? shape 'lines)
          (string-set! source (fx+ offset 1) #\newline))
        (loop (fx+ offset width))))
    source))

(def (main . args)
  (def (argument index default)
    (if (> (length args) index) (list-ref args index) default))
  (let* ((terms (string->number (argument 0 "1024")))
         (shape (string->symbol (argument 1 "lines")))
         (mode (string->symbol (argument 2 "artifact")))
         (samples (string->number (argument 3 "20")))
         (iterations (string->number (argument 4 "10"))))
    (unless (and (integer? terms) (positive? terms)
                 (memq shape '(lines terms))
                 (memq mode '(artifact recognition source-recognition))
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations))
      (error "expected size, lines/terms, artifact/recognition/source-recognition, samples, iterations"
             args))
    (let* ((source (addition-source terms shape))
           (runtime (parser-machine-runtime arithmetic-parser))
           (significant
            (and (eq? mode 'recognition)
                 (parser-significant-tokens
                  arithmetic-parser (lex-source arithmetic-parser source)))))
      (when (memq mode '(recognition source-recognition))
        (let* ((tokens (lex-source arithmetic-parser source))
               (input (parser-significant-tokens arithmetic-parser tokens)))
          (let-values (((root rest) (lr-parse/prepared runtime input)))
            (unless (and (null? rest)
                         (equal?
                          (parse-arithmetic-v1 source)
                          (make-success-parse-artifact
                           (parser-machine-grammar-digest arithmetic-parser)
                           source tokens root
                           (parser-machine-trivia arithmetic-parser))))
              (error "recognition does not match directed artifact" terms)))))
      (def (parse-one)
        (if (eq? mode 'artifact)
          (parse-arithmetic-v1 source)
          (let* ((input
                  (if significant
                    significant
                    (parser-significant-tokens
                     arithmetic-parser (lex-source arithmetic-parser source)))))
            (let-values (((root rest) (lr-parse/prepared runtime input)))
            (unless (null? rest)
              (error "prepared LR left unconsumed tokens" terms))
              root))))
      (def (check result)
        (when (eq? mode 'artifact)
          (unless (and (parse-artifact-success? result)
                       (string=? (parse-artifact-roundtrip result) source))
            (error "arithmetic ParseArtifact did not roundtrip" terms))))
      (write (list (cons 'input-shape shape) (cons 'terms terms)
                   (cons 'source-characters (string-length source))
                   (cons 'mode mode) (cons 'iterations iterations)))
      (newline)
      (let sample-loop ((sample -3))
        (when (< sample samples)
          (##gc)
          (let ((started (##current-time-point))
                (cpu-started (cpu-time)))
            (let parse-loop ((remaining iterations) (result #f))
              (if (zero? remaining)
                (let ((elapsed-ms
                       (/ (* 1000.0 (- (##current-time-point) started))
                          iterations))
                      (cpu-ms
                       (/ (* 1000.0 (- (cpu-time) cpu-started))
                          iterations)))
                  (check result)
                  (when (>= sample 0)
                    (write (list (cons 'sample sample)
                                 (cons 'elapsed-ms elapsed-ms)
                                 (cons 'cpu-ms cpu-ms)))
                    (newline)))
                (parse-loop (fx- remaining 1) (parse-one)))))
          (sample-loop (fx+ sample 1)))))))

(export main)
