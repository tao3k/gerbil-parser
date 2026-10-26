;;; External package-entry fixture; native SDK owns discovery and compilation.
(import (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool)
        (only-in :std/io/tempfile make-temporary-file-name))
(export query-spec)
;; Imports are not the only work after the native entry is expanded. Forward
;; Core's actual read/parse/projection events during spec projection too. Cache
;; hits, elapsed-time observations and request-only events cannot emit progress.
(def spec-source-progress
    '(begin
       (import (only-in :yuanxu/module-system/source/admission current-module-source-observer))
       (let ((prior (current-module-source-observer)) (completed 0))
         (current-module-source-observer
           (lambda (kind amount)
             (when prior (prior kind amount))
             (when (or (memq kind '(reads compiler-identity-reads role-parses
                                     publication-preparation-walks public-preparation-walks))
                       (and (pair? kind) (memq (car kind)
                         '(reader-projection-end reader-compile-end reader-eval-end))))
               (set! completed (+ completed 1))
               (when (or (= completed 1) (zero? (modulo completed 64)))
                 (display "BUILD-SPEC-SOURCE " (current-error-port))
                 (write completed (current-error-port))
                 (newline (current-error-port))
                 (force-output (current-error-port)))))))))
(def (query-spec root entry (noise? #f))
  ;; Native inherited stderr bypasses the Worker's Scheme output pipe. Keep
  ;; the spec in a private data file and use the existing streaming build-tool
  ;; adapter so actual diagnostics reach the existing idle observer.
  (let (spec-file (make-temporary-file-name "parser-build-spec-"))
    (try
      (run-build-tool
        (list "env" "-u" "GERBIL_LOADPATH" "gxi" "-:max-heap=1G,debug=q"
              "-e" (object->string
                `(begin
                   (import (only-in :gerbil-parser/src/build-support/compiler-profile
                                    call-with-compiler-import-progress))
                   ;; Reuse the actual compiler observer, including evaluator
                   ;; work, instead of a second import-only implementation.
                   (parameterize ((current-output-port (current-error-port)))
                     (call-with-compiler-import-progress
                       (lambda ()
                         (eval ',spec-source-progress)
                         (load ,(path-expand entry root))
                         (eval
                           '(begin
                              ,@(if noise?
                                  '((def actual-modules-spec modules-spec)
                                    (set! modules-spec
                                      (lambda (roots)
                                        (displayln "SPEC-PREPARATION-NOISE")
                                        (actual-modules-spec roots)))) '())
                              (call-with-output-file ,spec-file
                                (lambda (output)
                                  (parameterize ((current-output-port output))
                                    (main "spec"))))))))
                     (load-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
                     ((eval 'gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit!) 0)))))
        text?: #t)
      (call-with-input-file spec-file
        (lambda (input)
          (let (spec (read input))
            (unless (and (list? spec) (eof-object? (read input)))
              (error "native spec query did not publish one datum" entry))
            spec)))
      (finally (when (file-exists? spec-file) (delete-file spec-file))))))
