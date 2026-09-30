;;; -*- Gerbil -*-
;;; Matched AOT full-artifact HCL benchmark: indexed versus fused reductions.

(import (only-in :gerbil-parser/languages/hcl/v2-24/parser
                 hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-runtime parser-machine-trivia
                 parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-initial-checkpoint lr-checkpoint-drive)
        (only-in :gerbil-parser/src/runtime/lexer scan-source-token)
        (only-in :gerbil-parser/src/runtime/token token-end)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-success?))

(def (basic-source lines)
  (call-with-output-string
   (lambda (port)
     (let loop ((i 0))
       (when (< i lines)
         (display "key" port)
         (display i port)
         (display " = 1\n" port)
         (loop (fx+ i 1)))))))

(def (parse-source source generated?)
  (let ((machine hcl-v2-24-parser)
        (character-offset 0)
        (byte-offset 0)
        (pending-character #f)
        (tokens-reversed '()))
    (def (next-input mode)
      (if (= character-offset (string-length source))
        #f
        (let-values (((input-token next-character)
                      (scan-source-token machine source
                                         character-offset byte-offset mode)))
          (let (start-character character-offset)
            (set! character-offset next-character)
            (set! byte-offset (token-end input-token))
            (if ((parser-machine-trivia machine) input-token)
              (begin
                (set! tokens-reversed (cons input-token tokens-reversed))
                (next-input mode))
              (begin
                (set! pending-character start-character)
                input-token))))))
    (def (after-shift input-token _states _values _actions _shifts)
      (set! tokens-reversed (cons input-token tokens-reversed))
      (set! pending-character #f))
    (let-values (((status payload)
                  (lr-checkpoint-drive
                   (lr-initial-checkpoint
                    (parser-machine-runtime machine) '())
                   next-input after-shift #f
                   (if generated? 'installed #f))))
      (unless (and (eq? status 'accepted)
                   (= character-offset (string-length source))
                   (not pending-character)
                   (null? (cadr payload)))
        (error "HCL benchmark left deterministic path" status))
      (make-success-parse-artifact
       (parser-machine-grammar-digest machine)
       source (reverse tokens-reversed)
       (car payload) (parser-machine-trivia machine)))))

(def (main . args)
  (def (argument index default)
    (if (> (length args) index) (list-ref args index) default))
  (let* ((lines (string->number (argument 0 "1024")))
         (samples (string->number (argument 1 "20")))
         (iterations (string->number (argument 2 "10"))))
    (unless (and (integer? lines) (positive? lines)
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations))
      (error "expected lines, samples, iterations" args))
    (let* ((source (basic-source lines))
           (baseline (parse-source source #f))
           (generated (parse-source source #t))
           (public (parse-hcl-v2-24 source)))
      (unless (and (parse-artifact-success? baseline)
                   (equal? baseline generated)
                   (equal? generated public))
        (error "HCL benchmark changed ParseArtifact"))
      (def (measure generated?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (parse-source source generated?)
              (loop (fx- remaining 1))))
          (values
           (/ (* 1000.0 (- (cpu-time) cpu-started)) iterations)
           (/ (* 1000.0 (- (##current-time-point) started)) iterations))))
      (write (list (cons 'lines lines)
                   (cons 'source-characters (string-length source))
                   (cons 'iterations iterations)))
      (newline)
      (let sample-loop ((sample -3))
        (when (< sample samples)
          (let-values (((first-cpu first-elapsed)
                        (measure (odd? sample))))
            (let-values (((second-cpu second-elapsed)
                          (measure (even? sample))))
              (when (>= sample 0)
                (write
                 (list (cons 'sample sample)
                       (cons 'baseline-cpu-ms
                             (if (odd? sample) second-cpu first-cpu))
                       (cons 'generated-cpu-ms
                             (if (odd? sample) first-cpu second-cpu))
                       (cons 'baseline-elapsed-ms
                             (if (odd? sample) second-elapsed first-elapsed))
                       (cons 'generated-elapsed-ms
                             (if (odd? sample) first-elapsed second-elapsed))))
                (newline))))
          (sample-loop (fx+ sample 1)))))))
(export main)
