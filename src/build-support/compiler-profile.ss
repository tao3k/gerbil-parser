;;; Compiler execution and host policy shared by library and static owners.
(import (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string))
(export compiler-progress-expression compiler-c-options compiler-gsc-options compiler-build-spec run-build-tool compiler-host-options call-with-compiler-import-progress initialize-compiler-build!)

(def compiler-host-suffix
  " -Wp,-U___SINGLE_HOST,-D___MULTIPLE_HOSTS -v -Q -ftrack-macro-expansion=0")
(def (compiler-host-options configured)
  (if (string-suffix? compiler-host-suffix configured) configured
    (string-append configured compiler-host-suffix)))
(def (initialize-compiler-host!)
  ;; Gerbil intentionally removes -cc-options from macro-phase jobs. Use the
  ;; SDK's supported build environment for BOTH phases, preserving configured
  ;; flags verbatim. The SDK dump runs without compiling or changing anything.
  (let (configured
        (or (getenv "CORCXXFLAGS_GAMBUILD" #f)
            (run-process
              (list "/bin/sh" "-c"
                "profile=$1; shift; . \"$profile\" >/dev/null; printf '%s' \"$CORCXXFLAGS_GAMBUILD\""
                "parser-compiler-profile" (path-expand "bin/gambuild-C" (gerbil-home)))
              stdout-redirection: #t coprocess: read-all-as-string)))
    ;; -Wp is forwarded after the driver's collected -D options, including
    ;; the configured SINGLE_HOST define. No optimization level is replaced.
    (setenv "CORCXXFLAGS_GAMBUILD" (compiler-host-options configured))))
(def (initialize-compiler-build!)
  ;; Readers need policy data, not a compiler session. The installed SDK owns
  ;; capacity discovery; load and initialize it only for an actual build owner.
  (eval '(begin
           (import (only-in :asp-gerbil-scheme/src/build-api/core-capacity
                            initialize-native-build-core-capacity!))
           (initialize-native-build-core-capacity!)))
  (initialize-compiler-host!))

;; Report actual compiler GC work while large immutable images are lowered.
;; This changes the compiler process diagnostics, never generated program code.
(def compiler-progress-expression
  "(begin (port-settings-set! (current-output-port) (list buffering: #f)) (gc-report-set! #t))")

;; External tools start on independent pipes owned and drained by Gerbil.
;; Forward available bytes, including progress without a newline, before waiting.
(def (run-build-tool command text?: (text? #f))
  (run-process command stdout-redirection: #t stderr-redirection: #t
    coprocess:
    (lambda (process)
      (close-output-port process)
      (let (output (current-output-port))
        (if text?
          ;; The parser-test harness owns a string pipe. Decode incrementally
          ;; through the SDK port, including UTF-8 split between child writes.
          (let (buffer (make-string 8192))
            (let loop ()
              (let (count (read-substring buffer 0 (string-length buffer) process 1))
                (unless (zero? count)
                  (write-substring buffer 0 count output) (force-output output)
                  (loop)))))
          ;; Byte capture preserves arbitrary bytes without text conversion.
          (let (buffer (make-u8vector 8192))
            (let loop ()
              (let (count (read-subu8vector buffer 0 (u8vector-length buffer) process 1))
                (unless (zero? count)
                  (write-subu8vector buffer 0 count output) (force-output output)
                  (loop))))))))))

(def (compiler-c-options options)
  ;; The owned static closure includes dependencies as well as test modules.
  ;; Keep GCC optimization and the SDK's tail-call contract while compiling
  ;; each generated procedure as a separate C host. A single module host (for
  ;; example clan/poo/type) can spend the entire silence window in one GCC pass.
  (string-append options " -U___SINGLE_HOST -D___MULTIPLE_HOSTS"))
(def (compiler-gsc-options)
  (list "-verbose" "-cc-options" (compiler-c-options "-v -Q -ftrack-macro-expansion=0")
        "-e" compiler-progress-expression))

(def (compiler-build-spec spec)
  (map (lambda (item)
         (cond
           ((string? item) (append (list gxc: item) (compiler-gsc-options)))
           ((and (pair? item) (eq? (car item) gxc:))
            (append item (compiler-gsc-options)))
           ((and (pair? item) (memq (car item) (list gsc: exe: static-exe:)))
            ;; Executable/link drivers append their own GSC options; do not
            ;; put -e before those options. Keep the same GCC host contract.
            (append item (list "-verbose" "-cc-options" (compiler-c-options "-v -Q"))))
           (else item))) spec))

;;; std/make expands source interfaces before scheduling their compiler jobs.
;;; Report completed imports, not elapsed-time heartbeats, while that real work
;;; runs. Report each optimizer-interface boundary; a stuck import still
;;; trips the existing idle fence. Preserve the SDK importer and its failures.
(def (call-with-compiler-import-progress procedure)
  (let ((importer (gx#current-expander-module-import))
        (evaluate (gx#current-expander-module-eval))
        (completed 0) (started (##current-time-point))
        (trace (getenv "GERBIL_PARSER_LR_TRACE" #f)))
    (try
      (unless trace (setenv "GERBIL_PARSER_LR_TRACE" "1"))
      (parameterize
        ((gx#current-expander-module-import
          (lambda (path reload?)
            (let (context (importer path reload?))
              (set! completed (+ completed 1))
              (when (or (zero? (modulo completed 64))
                        (and (string? path) (string-suffix? ".ssxi.ss" path))
                        (and (symbol? path) (string-suffix? ".ssxi.ss" (symbol->string path))))
                (displayln "COMPILER-INTERFACES completed=" completed " last=" path
                           " elapsed-ms=" (inexact->exact (floor (* 1000 (- (##current-time-point) started)))))
                (force-output))
              context)))
         (gx#current-expander-module-eval
          (lambda (module)
            (let (name (if (or (gx#module-context? module) (gx#prelude-context? module))
                          (gx#expander-context-id module) module))
              (displayln "COMPILER-EVALUATE " name) (force-output)
              (call-with-values
                (lambda () (evaluate module))
                (lambda results
                  (displayln "COMPILER-EVALUATED " name) (force-output)
                  (apply values results)))))))
        (procedure))
      (finally
        (unless trace (setenv "GERBIL_PARSER_LR_TRACE"))))))
