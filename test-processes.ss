;;; Native qualification of process-owning test entries.
(displayln "PROCESS-QUALIFICATION-START") (force-output)
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(call-with-compiled-interface-trace
 (lambda () (eval '(import :gerbil-parser/t/support/qualification/process))))

(import :gerbil-parser/t/support/qualification/process)

(defstruct process-case (name arguments exit marker budget idle) transparent: #t)

(def (qualification-cases profile)
  (cond
   ((equal? profile "research")
    (let (binary (path-expand "bin/gerbil-parser-conformance" (getenv "GERBIL_PATH")))
      (map (lambda (name) (process-case name [binary name] 0 #f 10 5))
             '("concise-runtime" "history" "native-poo-runtime" "provenance"
               "all-language-benchmark"))))
   (else (error "unknown process qualification profile" profile))))

(def (qualify-process! case directory)
 (let* ((name (process-case-name case))
        (result (run-observed-process (process-case-arguments case)
                  (path-expand (string-append name ".log") directory)
                  timeout: (process-case-budget case) idle-timeout: (process-case-idle case)
                  error-output?: #f))
        (status (process-result-status result)) (expected (process-case-exit case)))
  (unless (and (not (process-result-reason result))
               (if (eq? expected 'nonzero) (> status 0) (= status expected)))
   ;; Preserve the last real child output in the job log as well as its artifact.
   ;; Hidden-directory upload defaults must never hide the failing native stage.
   (let* ((text (utf8->string (process-result-output result)))
          (end (string-length text)))
     (displayln "PROCESS-FAILURE " name " status=" status
                " reason=" (process-result-reason result)
                " phase=" (process-result-phase result) " elapsed=" (process-result-elapsed result))
     (displayln "PROCESS-FAILURE-TAIL " name)
     (display (substring text (max 0 (- end 8192)) end))
     (newline) (force-output))
   (error "unexpected process exit" name status (process-result-reason result)))
  (let (marker (process-case-marker case))
   (when (and marker (not (string-contains (utf8->string (process-result-output result)) marker)))
    (error "missing process receipt" name marker)))
  status))

(def (main profile . arguments)
  (let* ((cases (qualification-cases profile))
         (directory (if (pair? arguments) (car arguments)
                        (string-append ".data/" profile "-controls")))
         (prefix "RESEARCH-CONTROL-OK "))
    (create-directory* directory)
    (call-with-output-file (path-expand "exits.tsv" directory)
      (lambda (exits)
        (for-each (lambda (case)
                    (let (status (qualify-process! case directory))
                      (display (process-case-name case) exits) (display "\t" exits)
                      (display status exits) (newline exits)
                      (force-output exits)
                      (displayln prefix (process-case-name case)) (force-output))) cases)))
    (when (equal? profile "research")
      (displayln "DSL-RESEARCH-CLOSURE-OK: all child processes exited 0"))
    (displayln "OK") (force-output)
    (exit 0)))
