#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build! compiler-build-spec))
(import (only-in :clan/poo/object .get .call)
        (only-in "./init.ss" gerbil-parser.init.exports))
(export test-support-source shared-test-build-spec main)

;; Init owns discovery, native import closure and source currentness. This is
;; exactly the same native Suite roster as gerbil test t/... languages/... scripts/tests/... .
(def (test-support-source)
  (let* ((tests (.get gerbil-parser.init.exports tests))
         (files (append (.get (.call tests .for-directory "t") spec)
                        (.get (.call tests .for-directory "languages") spec)
                        (.get (.call tests .for-directory "scripts/tests") spec))))
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

;; Core's build projection already selects only native support imports, rather
;; than Suite roots. std/make compiles those helpers in the ordinary prefix;
;; gerbil test remains the sole Suite execution entry.
(def (shared-test-build-spec)
  (compiler-build-spec (.get (.get (test-support-source) build) spec)))
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
(defbuild-script (shared-test-build-spec) optimize: #t)
