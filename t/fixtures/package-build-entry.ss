;;; External package-entry fixture; native SDK owns discovery and compilation.
(import (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool)
        (only-in :std/io/tempfile make-temporary-file-name))
(export query-spec)
;; A spec query expands the actual Core/import closure before returning its
;; datum. Report completed SDK imports on the supervised diagnostic stream,
;; keeping spec data separate and the five-second silence deadline unchanged.
(def spec-import-progress
  (object->string
    '(begin
       (import :gerbil/expander)
       (let ((native-import (current-expander-module-import)) (completed 0))
         (current-expander-module-import
           (lambda (path reload?)
             (begin0 (native-import path reload?)
               (set! completed (+ completed 1))
               (when (zero? (modulo completed 64))
                 (display "BUILD-SPEC-IMPORTED " (current-error-port))
                 (write completed (current-error-port))
                 (newline (current-error-port))
                 (force-output (current-error-port))))))))))
(def (query-spec root entry (noise? #f))
  ;; Native inherited stderr bypasses the Worker's Scheme output pipe. Keep
  ;; the spec in a private data file and use the existing streaming build-tool
  ;; adapter so actual diagnostics reach the existing idle observer.
  (let (spec-file (make-temporary-file-name "parser-build-spec-"))
    (try
      (run-build-tool
        (list "env" "-u" "GERBIL_LOADPATH" "gxi" "-:max-heap=1G,debug=q" "-e" spec-import-progress
              "-L" (path-expand entry root)
              "-e" (object->string
                      `(begin
                         ,@(if noise?
                             '((def actual-modules-spec modules-spec)
                               (set! modules-spec
                                 (lambda (roots)
                                   (displayln "SPEC-PREPARATION-NOISE")
                                   (actual-modules-spec roots)))) '())
                         (call-with-output-file ,spec-file
                           (lambda (output)
                             (parameterize ((current-output-port output))
                               (main "spec"))))
                         ;; The datum is closed before the existing disposable
                         ;; test-child exit. Native teardown is not spec work.
                         (load-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
                         ((eval 'gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit!) 0))))
        text?: #t)
      (call-with-input-file spec-file
        (lambda (input)
          (let (spec (read input))
            (unless (and (list? spec) (eof-object? (read input)))
              (error "native spec query did not publish one datum" entry))
            spec)))
      (finally (when (file-exists? spec-file) (delete-file spec-file))))))
