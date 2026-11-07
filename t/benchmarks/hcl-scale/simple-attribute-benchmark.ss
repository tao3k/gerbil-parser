;;; -*- Gerbil -*-
;;; Compare the generated HCL simple-attribute corridor with its generic route.
;;; Compile with gxc -O before invoking main from gxi.

(import (only-in :gerbil-parser/languages/hcl/parser hcl-parser)
        (only-in :gerbil-parser/src/compiler/hcl-source
                 direct-parse-hcl)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?))

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
         (iterations (string->number (argument 2 "10")))
         (shape (argument 3 "simple")))
    (unless (and (integer? lines) (positive? lines)
                 (integer? samples) (positive? samples)
                 (integer? iterations) (positive? iterations)
                 (member shape '("simple" "late-complex")))
      (error "expected lines, samples, iterations, simple|late-complex" args))
    (let (source
          (if (equal? shape "simple")
            (basic-source lines)
            (string-append (basic-source lines) "complex = [1, 2]\n")))
      (let ((generic (direct-parse-hcl hcl-parser source #f))
            (candidate (direct-parse-hcl hcl-parser source)))
        (unless (and (parse-artifact-success? generic)
                     (equal? generic candidate))
          (error "HCL simple corridor changed complete ParseArtifact")))
      (def (measure simple?)
        (##gc)
        (let ((started (##current-time-point))
              (cpu-started (cpu-time)))
          (let loop ((remaining iterations))
            (when (> remaining 0)
              (direct-parse-hcl hcl-parser source simple?)
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
                       (cons 'generic-cpu-ms
                             (if (odd? sample) second-cpu first-cpu))
                       (cons 'corridor-cpu-ms
                             (if (odd? sample) first-cpu second-cpu))
                       (cons 'generic-elapsed-ms
                             (if (odd? sample) second-elapsed first-elapsed))
                       (cons 'corridor-elapsed-ms
                             (if (odd? sample) first-elapsed second-elapsed))))
                (newline))))
          (sample-loop (fx+ sample 1)))))))
(export main)
