#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (only-in :yuanxu/module-system/building/script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build!))
(import (only-in :clan/poo/object .get .call)
        (only-in "./init.ss" gerbil-parser.init.exports))
(export shared-test-build-spec main)

;; Init owns discovery, native import closure and source currentness. This is
;; exactly the same native Suite roster as gerbil test t/... languages/... .
(def (test-support-source product-sources)
  (let* ((tests (.get gerbil-parser.init.exports tests))
         (files (append (.get (.call tests .for-directory "t") spec)
                        (.get (.call tests .for-directory "languages") spec))))
    (.call tests .for-files
      (append files (map (lambda (file)
                           (path-expand file (path-directory (this-source-file))))
                         product-sources))
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
(def (shared-test-build-spec (product-sources '()))
  ((.get (test-support-source product-sources) std-make-spec)))
(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
(defbuild-script (shared-test-build-spec) optimize: #t)
