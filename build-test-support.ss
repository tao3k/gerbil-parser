#!/usr/bin/env gxi
;;; Standard currentness and scheduling for reusable Scheme test helpers.
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
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
                         product-sources)) '()
      ;; Only native options remain declared here; Core emits this target only
      ;; when the actual Suite import closure demands it.
      (list `(gxc: "t/benchmarks/gql/runtime/compiled-timing"
               ,@(cond-expand
                   (darwin '("-ld-options" "-Wl,-undefined,dynamic_lookup"))
                   (else '())))))))

(def (shared-test-build-spec (product-sources '()))
  (compiler-build-spec ((.get (test-support-source product-sources) std-make-spec))))
(defbuild-script (shared-test-build-spec) optimize: #t)
