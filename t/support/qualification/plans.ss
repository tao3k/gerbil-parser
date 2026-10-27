;;; Bounded runtime diagnostics consume the native writer's program products.
(import ./process ./products ./environment :std/misc/process :std/string/misc)
(export #t)
(defstruct qualification-stage (name command budget idle receipts) transparent: #t)
(def (compiled-product! root module)
 (let (product (path-expand (string-append "lib/gerbil-parser/" module ".o1") root))
  (unless (file-exists? product) (error "missing compiled module; build this checkout first" product))))
(def (execute-stage! stage logs)
 (displayln "NATIVE-STAGE " (qualification-stage-name stage)) (force-output)
 (run-qualified! (qualification-stage-command stage)
  (path-expand (string-append (qualification-stage-name stage) ".log") logs)
  timeout: (qualification-stage-budget stage) idle-timeout: (qualification-stage-idle stage)
  require: (qualification-stage-receipts stage)))
(def (local-diagnostic-plan suites prepared?)
 (unless (and (pair? suites) (andmap (lambda (suite) (member suite '("rust" "gql" "gql-profile" "gql-actors" "ffi" "aot"))) suites))
  (error "unknown local qualification suite" suites))
 (when (and prepared? (ormap (lambda (suite) (not (member suite '("gql-profile" "gql-actors")))) suites))
  (error "prepared diagnostics only admit profile and actor suites" suites))
 ;; Diagnostic programs are prepared by the native research writer, not by
 ;; rebuilding an independent helper list inside the runtime qualifier.
 (if (and (not prepared?)
          (ormap (lambda (suite) (member suite '("gql" "ffi"))) suites))
   [(qualification-stage "test-driver-build"
      '("gxi" "build-test-driver.ss" "compile") 240 30 '())]
   '()))
(def (qualify-local! suites prepared? compiled-root logs jobs)
 (let ((compiled-root (path-expand compiled-root)) (logs (path-expand logs)))
  (def (run name command (build? #f) (receipts '()) (budget #f))
   (execute-stage! (qualification-stage name command (or budget (if build? 240 120)) (if build? 30 5) receipts) logs))
  (native-environment!)
  (unless (and (integer? jobs) (> jobs 0)) (error "jobs must be positive" jobs))
  (setenv "GERBIL_PATH" compiled-root)
  (setenv "GERBIL_LOADPATH" (string-append (path-expand "lib" compiled-root) ":" (path-expand ".") ":" (path-expand ".")))
  (setenv "GAMBOPT" (string-append (getenv "GAMBOPT" "") ",max-heap=3G,debug=q"))
  (setenv "GERBIL_PARSER_LR_TRACE" "1") (setenv "GERBIL_BUILD_VERBOSE" "1")
  (setenv "RUSTC_WRAPPER") (setenv "RUSTC_WORKSPACE_WRAPPER")
  (for-each (lambda (stage) (execute-stage! stage logs)) (local-diagnostic-plan suites prepared?))
   (for-each
   (lambda (suite)
    (cond
     ((equal? suite "rust")
      (run "rust-build" ["cargo" "test" "--workspace" "--locked" "--no-run" "--jobs" (number->string jobs) "--verbose"] #t)
      (run "rust-test" '("cargo" "test" "--workspace" "--locked" "--" "--nocapture") #f '("test result: ok\\.")))
     ((member suite '("gql-profile" "gql-actors"))
      (run suite ["gerbil" "env"
                   (if (equal? suite "gql-profile")
                     "gerbil-parser-gql-profile" "gerbil-parser-gql-actors")
                   "40" "100"] #f
                 (if (equal? suite "gql-profile") '("GQL-STAGES-OK" "PARSER-COMPONENT-SUMMARY" "GQL-REDUCTION-COUNTS" "GQL-LR-EXECUTION")
                                                  '("GQL-ACTORS-OK" "PARSER-COMPONENT-SUMMARY"))))
     ((equal? suite "aot")
      (run "aot-build" '("gxi" "build-rust-runtime-aot.ss" "compile") #t)
      (with-product-directory "parser-native-aot-"
       (lambda (directory)
        (let (generated (path-expand "records.rs" directory))
         (run "aot-closed" ["env" "-i" (string-append "HOME=" (getenv "HOME")) "PATH=/usr/bin:/bin"
              (string-append "GERBIL_PATH=" compiled-root)
              (string-append "GERBIL_PARSER_RUNTIME_AOT_LIB=" (path-expand "lib" compiled-root) ":" (path-expand "lib" (gerbil-home)))
              (path-expand "bin/gerbil-parser-runtime-aot" compiled-root)
              "t/fixtures/runtime-record-assignments/languages/records/grammar.ss" generated])
         (run "aot-format" ["rustfmt" "--edition" "2024" generated])
         (same-product! generated "t/fixtures/runtime-record-assignments/src/generated/records.rs")))))
     (else
      (when (equal? suite "ffi")
       (run "ffi-header" [(getenv "CC" "cc") "-std=c11" "-Wall" "-Wextra" "-Werror" "-Wno-unused-command-line-argument"
                          "-fsyntax-only" "-Iinclude" "t/ffi-header-smoke.c"])
       (run "ffi-abi-build" '("gxi" "build-ffi-tests.ss" "compile") #t)
       (run "ffi-abi" ["gerbil" "env" "gerbil-parser-abi-probe"] #f
                     '("NATIVE-ABI-OK" "ABI-RUNTIME-GENERATED-MATCH calls=3"))
       (run "language-abi" ["gerbil" "env" "gerbil-parser-language-probe"] #f
                     '("LANGUAGE-ABI-OK" "LANGUAGE-ABI-100-CALLS"))
       (setenv "CARGO_TARGET_DIR" (path-expand "target"))
       (run "rust-native-build" '("cargo" "build" "--locked" "--manifest-path" "t/fixtures/rust-language-abi/Cargo.toml") #t '() 90)
       (setenv "GERBIL_PARSER_RUST_NATIVE_ARCHIVE" (path-expand "target/debug/librust_language_abi_fixture.a"))
       (run "rust-native-link" '("gxi" "build-rust-ffi-tests.ss" "compile") #t '() 90)
       (run "rust-native-ownership" ["gerbil" "env" "gerbil-parser-rust-language-probe"] #f
                     '("RUST-NATIVE-OK" "RUST-NATIVE-OWNERSHIP-OK handles=2 results=113" "RUST-NATIVE-100-CALLS") 90)
       (qualify-runtime-host!))
      (for-each (lambda (module) (compiled-product! compiled-root module))
                (if (equal? suite "gql") '("languages/gql/parser" "src/runtime/parser") '("src/ffi/rust-runtime-aot")))
      (let (files (if (equal? suite "gql")
                    '("t/gql/runtime-benchmark-test.ss" "t/gql/benchmark-profile-test.ss" "t/gql/actor-test.ss")
                    '("t/ffi-test.ss" "t/build-product-contract-test.ss" "t/language-abi-test.ss" "t/language-abi-benchmark-test.ss")))
       ;; Suite execution belongs directly to the native tool. Protocol probes
       ;; above retain their own idle/receipt checks; ordinary Suites do not.
       (run-process (append '("timeout" "--signal=TERM" "--kill-after=5s" "120s"
                              "gerbil" "test" "-v" "3") files)))))) suites)
  (displayln "LOCAL-NATIVE-OK " (string-join suites ",")) (force-output)))
