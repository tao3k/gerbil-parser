;;; -*- Gerbil -*-
;;; Compare the generated closed HCL lexical subset with the ranked lexer.
;;; Compile this module with gxc -O before invoking main from gxi.

(import (only-in :gerbil-parser/languages/hcl/parser hcl-parser)
        (only-in :gerbil-parser/languages/hcl/direct-recursive
                 direct-lex-hcl)
        (only-in :gerbil-parser/src/runtime/lexer lex-source))

(def (basic-source lines)
  (call-with-output-string
   (lambda (port)
     (let loop ((i 0))
       (when (< i lines)
         (display "key" port)
         (display i port)
         (display " = 1\n" port)
         (loop (fx+ i 1)))))))

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
    (let (source (basic-source lines))
      (let ((ranked (lex-source hcl-parser source))
            (generated (direct-lex-hcl source)))
        (unless (equal? ranked generated)
          (error "generated HCL lexer changed tokens")))
      (def (measure generated?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (if generated?
                (direct-lex-hcl source)
                (lex-source hcl-parser source))
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
                       (cons 'ranked-cpu-ms
                             (if (odd? sample) second-cpu first-cpu))
                       (cons 'generated-cpu-ms
                             (if (odd? sample) first-cpu second-cpu))
                       (cons 'ranked-elapsed-ms
                             (if (odd? sample) second-elapsed first-elapsed))
                       (cons 'generated-elapsed-ms
                             (if (odd? sample) first-elapsed second-elapsed))))
                (newline))))
          (sample-loop (fx+ sample 1)))))))
(export main)
