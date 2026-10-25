;;; Compiler execution and host policy shared by library and static owners.
(import (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string))
(export compiler-progress-expression compiler-c-options compiler-gsc-options compiler-build-spec run-build-tool compiler-host-options call-with-compiler-import-progress initialize-compiler-build!)

;; Keep real compiler/GC progress; GCC -Q dumps every generated function and
;; per-pass timing even for successful jobs. It is diagnostic output, not a
;; compiler semantic or optimization option.
(def compiler-host-suffix
  " -Wp,-U___SINGLE_HOST,-D___MULTIPLE_HOSTS -ftrack-macro-expansion=0")
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
  ;; A Homebrew SDK must default collect2 to Apple's linker even inside a Nix
  ;; shell. An explicit compiler search path remains the caller's selection.
  ;; Other SDK distributions retain their configured native toolchain policy.
  (cond-expand
    (darwin
      (when (and (not (getenv "COMPILER_PATH" #f))
                 (or (string-prefix? "/opt/homebrew/" (gerbil-home))
                     (string-prefix? "/usr/local/Cellar/" (gerbil-home))))
        (setenv "COMPILER_PATH" "/usr/bin")))
    (else (void)))
  ;; The SDK compiler and std/make read GERBIL_BUILD_CORES directly.
  (initialize-compiler-host!))

;; GC and SDK module jobs provide actual progress. Do not combine Gambit
;; -verbose function enumeration with this unbuffered diagnostic port: every
;; generated binding otherwise becomes synchronous build I/O.
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
  (list "-cc-options" (compiler-c-options "-ftrack-macro-expansion=0")
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
            (append item (list "-cc-options" (compiler-c-options ""))))
           (else item))) spec))

;;; std/make expands source interfaces before scheduling their compiler jobs.
;;; Report completed imports, not elapsed-time heartbeats, while that real work
;;; runs. Report each optimizer-interface boundary; a stuck import still
;;; trips the existing idle fence. Preserve the SDK importer and its failures.
(def (call-with-compiler-import-progress procedure (admit-import void))
  (let ((importer (gx#current-expander-module-import))
        (evaluate (gx#current-expander-module-eval))
        (completed 0) (started (##current-time-point))
        (admission-seconds 0.) (report-seconds 0.) (evaluation-seconds 0.)
        (evaluation-depth 0)
        (trace (getenv "GERBIL_PARSER_LR_TRACE" #f)))
    (def (report . fields)
      (def before (##current-time-point))
      (apply displayln fields) (force-output)
      (set! report-seconds (+ report-seconds (- (##current-time-point) before))))
    (try
      (unless trace (setenv "GERBIL_PARSER_LR_TRACE" "1"))
      (parameterize
        ((gx#current-expander-module-import
          (lambda (path reload?)
            ;; Admission precedes expansion and every initializer.
            (let (before (##current-time-point))
              (begin0 (admit-import path)
                (set! admission-seconds
                  (+ admission-seconds (- (##current-time-point) before)))))
            (let (context (importer path reload?))
              (set! completed (+ completed 1))
              (when (or (zero? (modulo completed 64))
                        (and (string? path) (string-suffix? ".ssxi.ss" path))
                        (and (symbol? path) (string-suffix? ".ssxi.ss" (symbol->string path))))
                (report "COMPILER-INTERFACES completed=" completed " last=" path
                           " elapsed-ms=" (inexact->exact (floor (* 1000 (- (##current-time-point) started))))
                           " admission-s=" admission-seconds " report-s=" report-seconds))
              context)))
         (gx#current-expander-module-eval
          (lambda (module)
            (let (name (if (or (gx#module-context? module) (gx#prelude-context? module))
                          (gx#expander-context-id module) module))
              (report "COMPILER-EVALUATE " name)
              (let ((before (##current-time-point)) (outer? (zero? evaluation-depth)))
                (set! evaluation-depth (+ evaluation-depth 1))
                (try
                  (call-with-values
                    (lambda () (evaluate module))
                    (lambda results
                      (report "COMPILER-EVALUATED " name)
                      (apply values results)))
                  (finally
                    (set! evaluation-depth (- evaluation-depth 1))
                    ;; Only outer evaluations count, never overlapping nested work.
                    (when outer?
                      (set! evaluation-seconds
                        (+ evaluation-seconds (- (##current-time-point) before)))))))))))
        (procedure))
      (finally
        (displayln "COMPILER-COST imports=" completed
          " admission-s=" admission-seconds " report-s=" report-seconds
          " outer-evaluation-s=" evaluation-seconds
          " total-s=" (- (##current-time-point) started))
        (force-output)
        (unless trace (setenv "GERBIL_PARSER_LR_TRACE"))))))
