;;; -*- Gerbil -*-
;;; Compare committed canonical HCL events with the raw event tape.
;;; Compile with gxc -O before invoking main from gxi.

(import (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-parser)
        (only-in :gerbil-parser/languages/hcl/v2-24/direct-recursive
                 direct-parse-hcl)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?))

(def (benchmark-source lines shape)
  (let (basic
        (call-with-output-string
         (lambda (port)
           (let loop ((i 0))
             (when (< i lines)
               (display "key" port)
               (display i port)
               (display " = 1\n" port)
               (loop (fx+ i 1)))))))
    (if (eq? shape 'late-complex)
      (string-append basic "array = [1, 2]\n")
      basic)))

(def (main . args)
  (def (argument index default)
    (if (> (length args) index) (list-ref args index) default))
  (let* ((lines (string->number (argument 0 "1024")))
         (samples (string->number (argument 1 "20")))
         (iterations (string->number (argument 2 "10")))
         (shape (string->symbol (argument 3 "basic"))))
    (unless (and (integer? lines) (positive? lines)
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations)
                 (memq shape '(basic late-complex)))
      (error "expected lines, samples, iterations, basic|late-complex"
             args))
    (let* ((source (benchmark-source lines shape))
           (raw (direct-parse-hcl hcl-v2-24-parser source #t #t #f))
           (canonical (direct-parse-hcl hcl-v2-24-parser source #t #t #t)))
      (unless (and (parse-artifact-success? raw)
                   (equal? raw canonical))
        (error "canonical path changed complete ParseArtifact" shape))
      (set! raw #f)
      (set! canonical #f)
      (def (measure canonical?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (direct-parse-hcl hcl-v2-24-parser source #t #t canonical?)
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
                       (cons 'raw-cpu-ms
                             (if (odd? sample) second-cpu first-cpu))
                       (cons 'canonical-cpu-ms
                             (if (odd? sample) first-cpu second-cpu))
                       (cons 'raw-elapsed-ms
                             (if (odd? sample) second-elapsed first-elapsed))
                       (cons 'canonical-elapsed-ms
                             (if (odd? sample) first-elapsed second-elapsed))))
                (newline))))
          (sample-loop (fx+ sample 1)))))))
(export main)
