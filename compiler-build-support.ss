;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Build-time process output and observation.
(import (only-in :std/misc/process run-process)
        (only-in :yuanxu/ops/progress copy-process-output))
(export run-build-tool call-with-compiler-import-progress)

;; External tools start on independent pipes owned and drained by Gerbil.
;; Forward available bytes, including progress without a newline, before waiting.
(def (run-build-tool command text?: (text? #f))
  (run-process command stdout-redirection: #t stderr-redirection: #t
    coprocess:
    (lambda (process)
      (copy-process-output process (current-output-port) #f #f
        mode: (if text? 'text 'bytes)))))

;;; std/make expands source interfaces before scheduling their compiler jobs.
;;; Report completed imports, not elapsed-time heartbeats, while that real work
;;; runs. Report each optimizer-interface boundary; a stuck import still
;;; trips the existing idle fence. Preserve the Gerbil importer and its failures.
(def (call-with-compiler-import-progress procedure (admit-import void))
  (let ((progress-output (current-output-port))
        (importer (gx#current-expander-module-import))
        (evaluate (gx#current-expander-module-eval))
        (completed 0) (started (##current-time-point))
        (admission-seconds 0.) (report-seconds 0.) (evaluation-seconds 0.)
        (evaluation-depth 0) (evaluation-start 0.) (cost-mutex (make-mutex 'compiler-cost))
        (trace (getenv "GERBIL_PARSER_LR_TRACE" #f)))
    (def (report . fields)
      (def before (##current-time-point))
      (parameterize ((current-output-port progress-output))
        (apply displayln fields) (force-output))
      (with-lock cost-mutex
        (lambda () (set! report-seconds (+ report-seconds (- (##current-time-point) before))))))
    (try
      (unless trace (setenv "GERBIL_PARSER_LR_TRACE" "1"))
      (parameterize
        ((gx#current-expander-module-import
          (lambda (path reload?)
            ;; Admission precedes expansion and every initializer.
            (let (before (##current-time-point))
              (begin0 (admit-import path)
                (with-lock cost-mutex
                  (lambda ()
                    (set! admission-seconds
                      (+ admission-seconds (- (##current-time-point) before)))))))
            (let (context (importer path reload?))
              (def count
                (with-lock cost-mutex
                  (lambda () (set! completed (+ completed 1)) completed)))
              ;; A batch of64 successful imports can span the idle window even
              ;; though no individual import is blocked. Report each completed
              ;; native operation; failed/in-flight imports still emit no completion.
              (report "COMPILER-INTERFACES completed=" count " last=" path
                         " elapsed-ms=" (inexact->exact (floor (* 1000 (- (##current-time-point) started))))
                         " admission-s=" admission-seconds " report-s=" report-seconds)
              context)))
         (gx#current-expander-module-eval
          (lambda (module)
            (let (name (if (or (gx#module-context? module) (gx#prelude-context? module))
                          (gx#expander-context-id module) module))
              (report "COMPILER-EVALUATE " name)
              ;; Count the union of active evaluations. Independent compiler
              ;; workers may overlap without being nested in one another.
              (with-lock cost-mutex
                (lambda ()
                  (when (zero? evaluation-depth) (set! evaluation-start (##current-time-point)))
                  (set! evaluation-depth (+ evaluation-depth 1))))
              (try
                  (call-with-values
                    (lambda () (evaluate module))
                    (lambda results
                      (report "COMPILER-EVALUATED " name)
                      (apply values results)))
                (finally
                  (with-lock cost-mutex
                    (lambda ()
                      (set! evaluation-depth (- evaluation-depth 1))
                      (when (zero? evaluation-depth)
                        (set! evaluation-seconds
                          (+ evaluation-seconds (- (##current-time-point) evaluation-start))))))))))))
        (procedure))
      (finally
        (parameterize ((current-output-port progress-output))
          (displayln "COMPILER-COST imports=" completed
            " admission-s=" admission-seconds " report-s=" report-seconds
            " evaluation-active-s=" evaluation-seconds
            " total-s=" (- (##current-time-point) started))
          (force-output))
        (unless trace (setenv "GERBIL_PARSER_LR_TRACE"))))))
