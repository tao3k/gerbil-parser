#!/usr/bin/env gxi
;;; Hold source/token binding constant and isolate complete artifact publication.
(import (only-in :gerbil-parser/languages/hcl/parser hcl-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-for-current-semantic-backend
                 parser-machine-grammar-digest parser-machine-trivia)
        (only-in :gerbil-parser/src/runtime/parser parse-source/checkpoints)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-event-program-enabled? current-lr-recognition-observer
                 lr-recognition-fragment-value lr-recognition-project)
        (only-in :gerbil-parser/src/runtime/recognition recognition-child-value)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence->list)
        (only-in :gerbil-parser/src/runtime/event-program event-program-value?)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact parse-artifact-events parse-artifact-valid?))
(def (root-value sequence)
  (let (children (recognition-sequence->list sequence))
    (unless (and (pair? children) (null? (cdr children)))
      (error "publication requires one root"))
    (recognition-child-value (car children))))
(def (median values)
  (let* ((sorted (list-sort < values)) (mid (quotient (length sorted) 2)))
    (/ (+ (list-ref sorted (- mid 1)) (list-ref sorted mid)) 2.0)))
(def (measure publish family units mode)
  (let (times
        (map (lambda (sample)
               (##gc)
               (let* ((start (cpu-time)) (artifact (publish))
                      (elapsed (* 1000.0 (- (cpu-time) start))))
                 (unless artifact (error "publication returned no artifact"))
                 (write (list 'publication-sample family units mode sample 'cpu-ms elapsed))
                 (newline) (force-output)
                 elapsed)) (iota 20)))
    (list (cons 'mode mode) (cons 'samples 20)
          (cons 'cpu-samples-ms times) (cons 'cpu-median-ms (median times)))))
(def (measure-source family units source)
  (let ((captured #f)
        (machine (parser-machine-for-current-semantic-backend hcl-parser)))
    (parameterize ((current-lr-recognition-observer (lambda (root) (set! captured root))))
      (let-values (((artifact tokens modes checkpoints)
                    (parse-source/checkpoints machine source 64)))
        (unless (and captured (parse-artifact-valid? artifact))
          (error "publication baseline lacks captured grammar"))
        (let* ((program (root-value (lr-recognition-fragment-value captured)))
               (canonical (root-value (lr-recognition-project captured tokens)))
               (publish (lambda (root)
                          (make-success-parse-artifact
                           (parser-machine-grammar-digest machine) source tokens root
                           (parser-machine-trivia machine))))
               (control (lambda () (publish canonical)))
               (candidate (lambda () (publish program))))
          (unless (and (event-program-value? program)
                       (equal? (control) artifact) (equal? (candidate) artifact))
            (error "publication roots differ from complete captured artifact"))
          (for-each
           (lambda (order)
             (for-each
              (lambda (mode)
                (write (append
                        (list (cons 'workload 'complete-semantic-publication)
                              (cons 'family family) (cons 'input-units units)
                              (cons 'order order) (cons 'complete-artifact-equal? #t)
                              (cons 'events (length (parse-artifact-events artifact))))
                        (measure (if (eq? mode 'canonical) control candidate) family units mode)))
                (newline) (force-output))
              (if (= order 1) '(canonical unified-event) '(unified-event canonical))))
           '(1 2)))))))
(def (main . args)
  (parameterize ((current-lr-event-program-enabled? #t))
    (for-each
     (lambda (units)
       (measure-source 'hcl-siblings units (string-join (make-list units "a = 1\n") "")))
     '(400 800))
    (for-each
     (lambda (units)
       (measure-source 'hcl-nested-siblings units
         (string-join (make-list units
           (string-append "resource \"r\" {\n" (string-join (make-list 80 "a = 1\n") "") "}\n")) "")))
     '(8 16)))
  (displayln "SEMANTIC-PUBLICATION-ALL-OK"))
(export main)
