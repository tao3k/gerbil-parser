(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/misc/process process-error?)
        (only-in :std/sync/wg make-wg wg-add! wg-wait!)
        (only-in :gerbil/expander current-expander-module-import current-expander-module-eval)
        (only-in :std/sync/channel make-channel channel-put channel-get)
        (only-in :std/text/pregexp pregexp-match)
        (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool compiler-host-options compiler-gsc-options initialize-compiler-build! call-with-compiler-import-progress))

(def (with-compiler-probe procedure)
  (let* ((root (make-temporary-file-name "parser-compiler-pipes-"))
         (probe (path-expand "probe" root)))
    (create-directory* root)
    (try
      (initialize-compiler-build!)
      (run-build-tool (list (getenv "GERBIL_GCC" "gcc")
                           "t/fixtures/compiler-pipe.c" "-o" probe) text?: #t)
      (procedure probe)
      (finally (delete-file-or-directory root #t)))))
(def (capture-build-bytes command)
  (call-with-output-u8vector
    (lambda (output)
      (parameterize ((current-output-port output)) (run-build-tool command)))))
(def (capture-build-text command)
  (call-with-output-string
    (lambda (output)
      (parameterize ((current-output-port output)) (run-build-tool command text?: #t)))))
(def (with-compiler-directory procedure)
  (let (root (make-temporary-file-name "parser-compiler-host-"))
    (create-directory* root)
    (try (procedure root)
         (finally (delete-file-or-directory root #t)))))

;; Synthetic crossed callbacks test interval bookkeeping, not compiler speed.
(def (overlapping-evaluation-output)
  (def started (make-channel))
  (def finish-a (make-channel))
  (def finish-b (make-channel))
  (call-with-output-string
    (lambda (output)
      (parameterize
        ((current-output-port output)
         (current-expander-module-eval
           (lambda (name)
             (channel-put started name)
             (channel-get (if (eq? name 'a) finish-a finish-b)) name)))
        (call-with-compiler-import-progress
          (lambda ()
            (let (a (spawn (lambda () ((current-expander-module-eval) 'a))))
              (channel-get started)
              (let (b (spawn (lambda () ((current-expander-module-eval) 'b))))
                (channel-get started)
                (channel-put finish-a #t) (thread-join! a)
                ;; B remains active after A, so lexical nesting alone loses it.
                (thread-sleep! .15)
                (channel-put finish-b #t) (thread-join! b)))))))))

;; The helper captures both streams; the declarative case emits no raw output.
(def (nested-data-output)
  (def datum #f)
  (def diagnostics
    (call-with-output-string
      (lambda (output)
        (parameterize
          ((current-output-port output)
           (current-expander-module-eval (lambda (_) '(selected module))))
          (call-with-compiler-import-progress
            (lambda ()
              (set! datum
                (call-with-output-string
                  (lambda (data)
                    (parameterize ((current-output-port data))
                      (write ((current-expander-module-eval) 'data-channel))))))))))))
  (values diagnostics datum))

;; Match the complete diagnostic record while allowing the clock's decimal or
;; exponent notation. Missing fields, malformed/negative costs and extra output
;; must not become acceptable by ignoring COMPILER-COST altogether.
(def (compiler-cost-output? output events)
  (let (match (pregexp-match
               (string-append "^" events
                 "COMPILER-COST imports=0 admission-s=([-+.0-9eE]+)"
                 " report-s=([-+.0-9eE]+) evaluation-active-s=([-+.0-9eE]+)"
                 " total-s=([-+.0-9eE]+)\n$") output))
    (and match (equal? (car match) output)
         (let loop ((fields (cdr match)))
           (or (null? fields)
               (let (number (string->number (car fields)))
                 (and (real? number) (>= number 0) (loop (cdr fields)))))))))

;; Compiler initialization changes the Gerbil's process-wide worker settings.

(deflanguage-parser-tests compiler-process-test "compiler process streams"
  (engine (exclusive #t))
  (scenario "compiler observation preserves values, exceptions and trace restoration"
    (equal (call-with-values
             (lambda () (call-with-compiler-import-progress (lambda () (values 'left 'right))))
             list)
           '(left right))
    (scope ((sentinel (list 'compiler-observation-refusal))
            (trace (getenv "GERBIL_PARSER_LR_TRACE" #f)))
      (equal (eq? (with-catch identity
                    (lambda () (call-with-compiler-import-progress (lambda () (raise sentinel)))))
                  sentinel) #t)
      (equal (getenv "GERBIL_PARSER_LR_TRACE" #f) trace)))
  (scenario "compiler diagnostics cannot corrupt a nested data channel"
    (scope-values (((diagnostics datum) (nested-data-output)))
      (equal (compiler-cost-output? diagnostics
               "COMPILER-EVALUATE data-channel\nCOMPILER-EVALUATED data-channel\n") #t)
      (equal datum "(selected module)")))
  (scenario "compiler observation retains admission before the protected native importer"
    (scope ((entered #f) (sentinel (list 'admission-refusal))
            (original (current-expander-module-import)))
      (parameters
        ((current-expander-module-import
           (lambda args (set! entered #t) (apply original args))))
        (equal (eq? (with-catch identity
                      (lambda ()
                        (call-with-compiler-import-progress
                          (lambda () ((current-expander-module-import) ':gerbil/core #f))
                          (lambda (_) (raise sentinel))))) sentinel) #t))
      (equal entered #f)))
  (scenario "overlapping evaluation accounting retains the trailing worker interval"
    (scope ((output (overlapping-evaluation-output)))
      (scope ((match (pregexp-match "evaluation-active-s=([-+.0-9eE]+)" output)))
        (equal (and match (>= (string->number (cadr match)) .14) #t) #t))))
  (scenario "byte output retains pressure fragments and arbitrary bytes"
    (using (with-compiler-probe) (probe)
      (equal (capture-build-bytes (list probe "--burst")) (make-u8vector 262144 120))
      (equal (capture-build-bytes (list probe "--fragment")) (string->utf8 "fragmentdiagnostic"))
      (equal (capture-build-bytes (list probe "--bytes")) '#u8(0 255 195 128 10))))
  (scenario "text diagnostics decode UTF-8 split across child writes"
    (using (with-compiler-probe) (probe)
      (equal (capture-build-text (list probe "--utf8-fragment")) "λ")))
  (scenario "the configured GCC profile overrides a later Gerbil host define without changing optimization"
    (scope ((flags (compiler-host-options "-O2 -pipe")))
      (equal (string-prefix? "-O2 -pipe " flags) #t)
      (equal (compiler-host-options flags) flags)
      (equal (member "-v" (string-split flags #\space)) #f)
      (equal (member "-Q" (string-split flags #\space)) #f)
      (equal (member "-verbose" (compiler-gsc-options)) #f)
      (scope ((macros (capture-build-text
                        (append (list (getenv "GERBIL_GCC" "gcc"))
                                (string-split flags #\space)
                                '("-D___SINGLE_HOST" "-x" "c" "-E" "-dM" "/dev/null")))))
        (equal (and (string-contains macros "#define ___MULTIPLE_HOSTS 1") #t) #t)
        (equal (string-contains macros "#define ___SINGLE_HOST") #f))))
  (scenario "importing compiler policy does not initialize a compiler session"
    (equal (capture-build-text
             (list "gxi" "-e"
               (object->string
                '(begin
                   (setenv "GERBIL_BUILD_CORES")
                   (setenv "CORCXXFLAGS_GAMBUILD")
                   (eval '(import :gerbil-parser/src/build-support/compiler-profile))
                   (write (list (getenv "GERBIL_BUILD_CORES" #f)
                                (getenv "CORCXXFLAGS_GAMBUILD" #f)))))))
           "(#f #f)"))
  (scenario "native linker defaults preserve explicit compiler search paths"
    (equal (capture-build-text
             (list "gxi" "-e"
               (object->string
                '(begin
                   (setenv "COMPILER_PATH" "/explicit/compiler-tools")
                   (setenv "CORCXXFLAGS_GAMBUILD" "-O2")
                   (eval '(import :gerbil-parser/src/build-support/compiler-profile))
                   ((eval 'initialize-compiler-build!))
                   (write (getenv "COMPILER_PATH" #f))))))
           (object->string "/explicit/compiler-tools")))
  (scenario "compiler evaluation diagnostics preserve zero and multiple values"
    (each (expected) ('(() (first second)))
      (scope ((actual #f))
        (equal
          (compiler-cost-output?
            (call-with-output-string
              (lambda (output)
                (parameterize
                  ((current-output-port output)
                   (gx#current-expander-module-eval (lambda (_) (apply values expected))))
                  (set! actual
                    (call-with-values
                      (lambda ()
                        (call-with-compiler-import-progress
                          (lambda () ((gx#current-expander-module-eval) 'probe))))
                      list)))))
            "COMPILER-EVALUATE probe\nCOMPILER-EVALUATED probe\n")
          #t)
        (equal actual expected))))
  (scenario "compiler cost diagnostics reject incomplete or invalid records"
    (scope ((events "COMPILER-EVALUATE probe\nCOMPILER-EVALUATED probe\n")
            (cost "COMPILER-COST imports=0 admission-s=0. report-s=1e-6 evaluation-active-s=.2 total-s=2.1E-1\n"))
      (equal (compiler-cost-output? (string-append events cost) events) #t)
      (each (invalid)
        ((list events
               (string-append events "COMPILER-COST imports=0\n")
               (string-append events "COMPILER-COST imports=0 admission-s=e report-s=0. evaluation-active-s=0. total-s=0.\n")
               (string-append events "COMPILER-COST imports=0 admission-s=-1 report-s=0. evaluation-active-s=0. total-s=0.\n")
               (string-append events cost cost)
               (string-append events cost "unexpected output\n")))
        (equal (compiler-cost-output? invalid events) #f))))
  (scenario "compiler evaluation exceptions retain diagnostics and restore callbacks"
    (scope ((sentinel (list 'evaluation-refusal))
            (importer (current-expander-module-import))
            (evaluate (current-expander-module-eval))
            (actual #f))
      (equal
        (compiler-cost-output?
          (call-with-output-string
            (lambda (output)
              (parameterize
                ((current-output-port output)
                 (current-expander-module-eval (lambda (_) (raise sentinel))))
                (set! actual
                  (with-catch identity
                    (lambda ()
                      (call-with-compiler-import-progress
                        (lambda () ((current-expander-module-eval) 'probe)))))))))
          "COMPILER-EVALUATE probe\n")
        #t)
      (equal (eq? actual sentinel) #t)
      (equal (eq? (current-expander-module-import) importer) #t)
      (equal (eq? (current-expander-module-eval) evaluate) #t)))
  (scenario "qualification runtime services do not request compiler interfaces"
    (equal (capture-build-text
             (list "gxi" "-e"
               (object->string
                 '(parameterize
                    ((gx#current-expander-module-import
                       (lambda args (error "runtime CLI requested a compiler interface"))))
                    (load "qualify.ss")
                    (unless (and
                      (procedure? (eval 'run-observed-process))
                      (procedure? ((eval 'profile-value)
                        "gerbil-parser/t/support/qualification/process"
                        'gerbil-parser/t/support/qualification/process#receipt-present?)))
                      (error "runtime qualification service was not loaded"))))))
           "QUALIFICATION-START\n"))
  (scenario "compiler children inherit the active Gerbil logical bin mapping"
    (equal (capture-build-text
             (list (getenv "GERBIL_GSC" (path-expand "bin/gsc" (gerbil-home))) "-e" "(write (path-expand \"~~bin\"))"))
           (object->string (path-expand "~~bin"))))
  (scenario "Gerbil compilation without per-job C options loads and executes its native result"
    (using (with-compiler-directory) (root)
      (act (initialize-compiler-build!))
      (scope ((output (path-expand "compiler-host.o1" root)))
        (act (run-build-tool (list (getenv "GERBIL_GSC" (path-expand "bin/gsc" (gerbil-home))) "-verbose" "-o" output
                                  "t/fixtures/compiler-host.scm") text?: #t))
        (equal (file-exists? output) #t)
        (equal (capture-build-text
                 (list "gxi" "-e"
                   (string-append "(load " (object->string output)
                                  ") (write (compiler-host-probe 41))"))) "42"))))
  (scenario "concurrent compiler children retain blocking pipes and propagate failure"
    (using (with-compiler-probe) (probe)
      (scope ((workers (make-wg 4)) (results (make-vector 8 #f)))
        (each (job) ((iota 8))
          (act (wg-add! workers (lambda ()
                  (vector-set! results job (capture-build-bytes (list probe)))))))
        (act (wg-wait! workers))
        (each (result) ((vector->list results))
          (equal result (string->utf8 "COMPILER-PIPE-OK\n"))))
      (raises (run-build-tool (list probe "--failure") text?: #t) process-error?))))
