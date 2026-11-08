#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (for-syntax
          (only-in :std/source this-source-file)
          (only-in :yuanxu/module-system/building/source-package register-module-build-source!)))
(begin-syntax
  (register-module-build-source! (path-expand "../../../" (path-directory (this-source-file))) 'gerbil-parser))
(import (rename-in (only-in :gerbil/tools/gxtest main) (main gxtest-main))
        (only-in :std/build-script defbuild-script)
        (only-in :yuanxu/module-system/building/source-package
                 call-with-build-input-path register-module-build-source!)
        (only-in :std/source this-source-file)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in "../../fixtures/ffi/build.ss" ffi-build-spec ffi-test-command)
        (rename-in (only-in "../../../src/ffi/build.ss" Build) (Build ffi-library-build))
        (only-in :yuanxu/observability/testing-script TestingScript.)
        (only-in :yuanxu/observability/testing-fixture call-with-yuanxu-fixture))
(import (only-in :clan/poo/object .o .cc .get .call)
        (only-in "../../../init.ss" gerbil-parser.init.exports))
(extern namespace: gerbil/tools/gxtest collect-files)
(export run-native-tests main)
(register-module-build-source! (path-expand "../../../" (path-directory (this-source-file))) 'gerbil-parser)

(def (test-support-targets files)
  (def (qualify-ffi-target target)
    (cons (car target) (cons (string-append "src/ffi/" (cadr target)) (cddr target))))
  (def timing
    `(gxc: "t/benchmarks/gql/runtime/compiled-timing"
       ,@(cond-expand
           (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
           (else '()))))
  ;; Retained inputs are relative to this package. The production FFI
  ;; declaration owns the options, including those for its compiler leaves.
  (def (support-library? target)
    (not (member (string-append (cadr target) ".ss") files)))
  (delete-duplicates/hash
    (append (filter support-library? (ffi-build-spec #t))
            (map qualify-ffi-target (.get ffi-library-build roots))
            (list timing))
    key: cadr))

;; GxTest discovers the complete roster. Init binds those files to the
;; native import closure and source currentness.
(def package-source-file (path-expand "../../../build.ss" (path-directory (this-source-file))))

(def (test-support-source)
  (let* ((tests (.get gerbil-parser.init.exports tests))
         (files (append (.get (.call tests .for-directory "t") spec)
                        (.get (.call tests .for-directory "languages") spec)
                        (.get (.call tests .for-directory "scripts/tests") spec))))
    (.call tests .for-files
      files
      (list (this-source-file)
            (path-expand "compiler-build-support.ss"
                         (path-directory package-source-file))
            (path-expand "t/fixtures/ffi/build.ss" (path-directory package-source-file))
            (path-expand "src/ffi/build.ss" (path-directory package-source-file)))
      ;; The test projection recompiles reached libraries into its own output.
      ;; Retain the owning build declarations, including native FFI options.
      (test-support-targets files))))

(def (run-native-tests)
  (def demand (test-support-source))
  (def build (.get demand build))
  (def admit! (.get build admit-artifacts))
  (def prior (getenv "GERBIL_LOADPATH" ""))
  (create-directory* ".data")
  (def script
    (.o (:: @ [TestingScript.])
      source: demand
      profile: (.cc (.get TestingScript. profile)
                 heap-limit-bytes: 3221225472 max-duration-milliseconds: 900000)
      environment: (.o GERBIL_LOADPATH:
        (string-append (.get build output-directory)
          (if (string-empty? prior) "" (string-append ":" prior))))
      idle-timeout: #f
      log: ".data/native-gerbil-test.log"))
  (call-with-yuanxu-fixture
    (.o .acquire: admit! .release: (lambda (_) (admit!)))
    (lambda (_)
      (def command (ffi-test-command (.get script command) (.get script runtime-options)))
      (display (.get (.call (.cc script command: command) .run) output)))))

;; Yuanxu automatically dispatches executable test targets. This support
;; package contains library targets, so request issuance on the projection
;; explicitly and let the standard build script compile into its output.
(def (build-support-spec spec output arguments)
  (def (this-source-file) package-source-file)
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
