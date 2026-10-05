;;; -*- Gerbil -*-
;;; Compare generated ASCII HCL comment lexing with ranked lexing.
;;; Compile with gxc -O before invoking main from gxi.

(import (only-in :gerbil-parser/languages/hcl/parser hcl-parser)
        (only-in :gerbil-parser/languages/hcl/direct-recursive
                 direct-parse-hcl direct-lex-hcl)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?))

(def (comment-source lines shape)
  (call-with-output-string
   (lambda (port)
     (let loop ((i 0))
       (when (< i lines)
         (display "key" port)
         (display i port)
         (case shape
           ((hash) (display " = 1 # note\n" port))
           ((slash) (display " = 1 // note\n" port))
           ((block) (display " = 1 /* note */\n" port))
           ((late-unicode)
            (if (= i (fx- lines 1))
              (display " = 1 # 雪\n" port)
              (display " = 1 # note\n" port))))
         (loop (fx+ i 1)))))))

(def (main . args)
  (def (argument index default)
    (if (> (length args) index) (list-ref args index) default))
  (let* ((lines (string->number (argument 0 "1024")))
         (samples (string->number (argument 1 "20")))
         (iterations (string->number (argument 2 "10")))
         (shape (string->symbol (argument 3 "hash"))))
    (unless (and (integer? lines) (positive? lines)
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations)
                 (memq shape '(hash slash block late-unicode)))
      (error "expected lines, samples, iterations, hash|slash|block|late-unicode"
             args))
    (let* ((source (comment-source lines shape))
           (fast-tokens (direct-lex-hcl source))
           (ranked (direct-parse-hcl hcl-parser source #t #f))
           (candidate (direct-parse-hcl hcl-parser source #t #t)))
      (unless (and (parse-artifact-success? ranked)
                   (equal? ranked candidate)
                   (if (eq? shape 'late-unicode)
                     (not fast-tokens)
                     (and fast-tokens
                          (equal? fast-tokens
                                  (lex-source hcl-parser source)))))
        (error "HCL comment path changed tokens or ParseArtifact"))
      (set! fast-tokens #f)
      (set! ranked #f)
      (set! candidate #f)
      (def (measure fast?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (direct-parse-hcl hcl-parser source #t fast?)
              (loop (fx- remaining 1))))
          (values
           (/ (* 1000.0 (- (cpu-time) cpu-started)) iterations)
           (/ (* 1000.0 (- (##current-time-point) started)) iterations))))
      (write (list (cons 'lines lines)
                   (cons 'shape shape)
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
                       (cons 'ranked-cpu-ms
                             (if (odd? sample) second-cpu first-cpu))
                       (cons 'candidate-cpu-ms
                             (if (odd? sample) first-cpu second-cpu))
                       (cons 'ranked-elapsed-ms
                             (if (odd? sample) second-elapsed first-elapsed))
                       (cons 'candidate-elapsed-ms
                             (if (odd? sample) first-elapsed second-elapsed))))
                (newline))))
          (sample-loop (fx+ sample 1)))))))
(export main)
