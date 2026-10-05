;;; -*- Gerbil -*-
;;; Appended to generated LR source to make a standalone matched AOT benchmark.

(import (only-in :gerbil-parser/src/runtime/lexer
                 lex-source scan-source-token)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 parse-arithmetic))

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

(def validation-sources
  '("1" "123" "a" "a+b" "1+2*3" "(1+2)*3" "-1+2"
    "-(1+2)" "+a" "1-2-3" "1/(2+3)" "(-1)" "a*b/c"
    "1 +\n 2 +\n 3" "(a + b) * (c - d)" "1+2*3-4/5"
    "((((1))))" "a + -b" "-a*-b" "1\n+\n2"
    "π+1" "α + β" "1 + 中" "é+2"))

(def (parse-source/fused source)
  (let ((source-length (string-length source))
        (trivia? (parser-machine-trivia arithmetic-parser))
        (runtime (parser-machine-runtime arithmetic-parser))
        (character-offset 0)
        (byte-offset 0)
        (pending-character #f)
        (tokens-reversed '()))
    (def (next-input mode)
      (if (= character-offset source-length)
        #f
        (let-values (((input-token next-character)
                      (scan-source-token arithmetic-parser source
                                         character-offset byte-offset mode)))
          (let (start-character character-offset)
            (set! character-offset next-character)
            (set! byte-offset (token-end input-token))
            (if (trivia? input-token)
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
                  (direct-drive runtime next-input after-shift)))
      (unless (and (eq? status 'accepted)
                   (= character-offset source-length)
                   (not pending-character)
                   (null? (cadr payload)))
        (error "generated streaming LR left its fast path" status source))
      (make-success-parse-artifact
       (parser-machine-grammar-digest arithmetic-parser)
       source (reverse tokens-reversed) (car payload) trivia?))))

(def (assert-same-artifact source)
  (let* ((tokens (lex-source arithmetic-parser source))
         (input (parser-significant-tokens arithmetic-parser tokens))
         (runtime (parser-machine-runtime arithmetic-parser)))
    (let-values (((reference reference-rest)
                  (lr-parse/prepared runtime input))
                 ((generated generated-rest)
                  (direct-parse input #t)))
      (unless (and (null? reference-rest)
                   (null? generated-rest)
                   (equal? reference generated)
                   (equal?
                    (parse-source/fused source)
                    (parse-arithmetic source))
                   (equal?
                    (parse-arithmetic source)
                    (make-success-parse-artifact
                     (parser-machine-grammar-digest arithmetic-parser)
                     source tokens generated
                     (parser-machine-trivia arithmetic-parser))))
        (error "generated LR differs from the complete artifact" source)))))

(def (main . args)
  (def (argument index default)
    (if (> (length args) index) (list-ref args index) default))
  (let* ((terms (string->number (argument 0 "1024")))
         (shape (string->symbol (argument 1 "lines")))
         (mode (string->symbol (argument 2 "prepared")))
         (samples (string->number (argument 3 "20")))
         (iterations (string->number (argument 4 "10"))))
    (unless (and (integer? terms) (positive? terms)
                 (memq shape '(lines terms))
                 (memq mode '(prepared source artifact))
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations))
      (error "expected size, lines/terms, prepared/source/artifact, samples, iterations"
             args))
    (let* ((source (addition-source terms shape))
           (input (parser-significant-tokens
                   arithmetic-parser (lex-source arithmetic-parser source)))
           (runtime (parser-machine-runtime arithmetic-parser)))
      (for-each assert-same-artifact validation-sources)
      (assert-same-artifact source)
      (def (parse-one generated?)
        (if (eq? mode 'artifact)
          (values (if generated?
                    (parse-source/fused source)
                    (parse-arithmetic source))
                  '())
          (let (prepared
                (if (eq? mode 'prepared)
                  input
                  (parser-significant-tokens
                   arithmetic-parser (lex-source arithmetic-parser source))))
            (if generated?
              (direct-parse prepared #t)
              (lr-parse/prepared runtime prepared)))))
      (def (measure generated?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (let-values (((_root rest) (parse-one generated?)))
                (unless (null? rest)
                  (error "LR left unconsumed tokens" mode)))
              (loop (fx- remaining 1))))
          (values
           (/ (* 1000.0 (- (cpu-time) cpu-started)) iterations)
           (/ (* 1000.0 (- (##current-time-point) started)) iterations))))
      (write (list (cons 'terms terms) (cons 'shape shape)
                   (cons 'source-characters (string-length source))
                   (cons 'mode mode) (cons 'iterations iterations)))
      (newline)
      (let sample-loop ((sample -3))
        (when (< sample samples)
          (let-values (((first-cpu first-elapsed)
                        (measure (odd? sample))))
            (let-values (((second-cpu second-elapsed)
                          (measure (even? sample))))
              (when (>= sample 0)
                (let ((baseline-cpu (if (odd? sample) second-cpu first-cpu))
                      (baseline-elapsed
                       (if (odd? sample) second-elapsed first-elapsed))
                      (generated-cpu (if (odd? sample) first-cpu second-cpu))
                      (generated-elapsed
                       (if (odd? sample) first-elapsed second-elapsed)))
                  (write
                   (list (cons 'sample sample)
                         (cons 'baseline-cpu-ms baseline-cpu)
                         (cons 'generated-cpu-ms generated-cpu)
                         (cons 'baseline-elapsed-ms baseline-elapsed)
                         (cons 'generated-elapsed-ms generated-elapsed)))
                  (newline)))))
          (sample-loop (fx+ sample 1)))))))

(export main)
