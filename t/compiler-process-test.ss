(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/misc/process process-error?)
        (only-in :std/sync/wg make-wg wg-add! wg-wait!)
        (only-in :gerbil-parser/src/build-support/compiler-profile run-build-tool compiler-host-options initialize-compiler-build! call-with-compiler-import-progress))

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

;; Compiler initialization changes the SDK's process-wide worker settings.

(deflanguage-parser-tests compiler-process-test "compiler process streams"
  (engine (exclusive #t))
  (scenario "byte output retains pressure fragments and arbitrary bytes"
    (using (with-compiler-probe) (probe)
      (equal (capture-build-bytes (list probe "--burst")) (make-u8vector 262144 120))
      (equal (capture-build-bytes (list probe "--fragment")) (string->utf8 "fragmentdiagnostic"))
      (equal (capture-build-bytes (list probe "--bytes")) '#u8(0 255 195 128 10))))
  (scenario "text diagnostics decode UTF-8 split across child writes"
    (using (with-compiler-probe) (probe)
      (equal (capture-build-text (list probe "--utf8-fragment")) "λ")))
  (scenario "the configured GCC profile overrides a later SDK host define without changing optimization"
    (scope ((flags (compiler-host-options "-O2 -pipe")))
      (equal (string-prefix? "-O2 -pipe " flags) #t)
      (equal (compiler-host-options flags) flags)
      (equal (member "-Q" (string-split flags #\space)) #f)
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
        (equal actual expected))))
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
  (scenario "compiler children inherit the active SDK logical bin mapping"
    (equal (capture-build-text
             (list (getenv "GERBIL_GSC" (path-expand "bin/gsc" (gerbil-home))) "-e" "(write (path-expand \"~~bin\"))"))
           (object->string (path-expand "~~bin"))))
  (scenario "SDK compilation without per-job C options loads and executes its native result"
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
