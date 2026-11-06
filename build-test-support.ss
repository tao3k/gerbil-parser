#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser))
(import (rename-in (only-in :gerbil/tools/gxtest main) (main gxtest-main))
        (only-in :std/build-script defbuild-script)
        (only-in :yuanxu/module-system/building/source-package
                 call-with-build-input-path register-module-build-source!)
        (only-in :std/source this-source-file)
        (only-in :yuanxu/observability/testing-script TestingScript.)
        (only-in :yuanxu/observability/testing-fixture call-with-yuanxu-fixture)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build!))
(import (only-in :clan/poo/object .o .cc .get .call)
        (only-in "./init.ss" gerbil-parser.init.exports))
(extern namespace: gerbil/tools/gxtest collect-files)
(export test-support-source shared-test-build-spec run-native-tests main)
(register-module-build-source! (path-directory (this-source-file)) 'gerbil-parser)

;; GxTest discovers the complete roster. Init binds those files to the
;; native import closure and source currentness.
(def (test-support-source)
  (let* ((tests (.get gerbil-parser.init.exports tests))
         (files (collect-files ["t/..." "languages/..."])))
    (.call tests .for-files
      files
      (list (this-source-file)
            (path-expand "src/build-support/compiler-profile.ss"
                         (path-directory (this-source-file))))
      ;; Only native options remain declared here; Core emits this target only
      ;; when the actual Suite import closure demands it.
      (list `(gxc: "t/benchmarks/gql/runtime/compiled-timing"
               ,@(cond-expand
                   (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                   (else '())))))))

;; Preserve the exact Core-produced value so its native consumer retains
;; source/artifact admission and subset publication. Compiler host policy is
;; the same process-local setup used by the production build entry.
(def (shared-test-build-spec)
  ((.get (test-support-source) std-make-spec)))
(def (run-native-tests)
  (def source (test-support-source))
  (def build (.get source build))
  (def admit! (.get build admit-artifacts))
  (def prior (getenv "GERBIL_LOADPATH" ""))
  (def script
    (.o (:: @ [TestingScript.])
      source: source
      profile: (.cc (.get TestingScript. profile)
                 heap-limit-bytes: 3221225472 max-duration-milliseconds: 900000)
      environment: (.o GERBIL_LOADPATH:
        (string-append (.get build output-directory)
          (if (string-empty? prior) "" (string-append ":" prior))))
      idle-timeout: #f
      log: ".data/native-gerbil-test.log"))
  (call-with-yuanxu-fixture
    (.o .acquire: admit! .release: (lambda (_) (admit!)))
    (lambda (_) (display (.get (.call script .run) output)))))

(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
;; Yuanxu automatically dispatches executable test targets. This support
;; package contains library targets, so request issuance on the projection
;; explicitly and let the standard build script compile into its output.
(def (build-support-spec spec output arguments)
  (defbuild-script spec optimize: #t libdir: output)
  (apply main arguments))

(def (main . arguments)
  (def build (.get (test-support-source) build))
  (def spec (.get build spec))
  (def output (.get build output-directory))
  (cond
   ((or (null? arguments) (equal? (car arguments) "compile"))
    (.call build .compile output
      (lambda ()
        (def destination ((.get build compile-output)))
        (call-with-build-input-path destination
          (lambda ()
            (build-support-spec spec destination arguments))
          make-source: (.get build source-root)))))
   (else (build-support-spec spec output arguments))))
