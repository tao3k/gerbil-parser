#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Prepare the declared native gxtest closure through the existing PackageSpec.
(import (only-in :clan/poo/object .o)
        (only-in :std/make make)
        (only-in :std/misc/process run-process)
        (only-in :std/string/misc string-trim)
        (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-library-package-prototype
                 asp-gerbil-scheme-package-policy-native-spec))
(export main)
(def tests
  (.o (:: @ asp-gerbil-scheme-library-package-prototype)
      public-entry-modules:
      '("t/runtime-module-organization-test.ss"
        "t/test-style-contract-test.ss"
        "t/benchmarks/parser-throughput/link-locks-test.ss"
        "t/check-runtime-organization.ss"
        "t/lr-semantic-preparation-test.ss"
        "t/lr-stack-reduction-test.ss"
        "t/lr-action-preparation-test.ss"
        "t/lr-fragment-transfer-test.ss"
        "t/recovery-incremental-test.ss")))
(def (main)
  (let (spec (asp-gerbil-scheme-package-policy-native-spec tests))
    (make spec srcdir: (current-directory)
          libdir: (path-expand "lib" (getenv "GERBIL_PATH"))
          optimize: #t parallelize: 4)
    (make (list (list 'exe: "t/runtime-organization-closure.ss"
                      bin: (path-expand "bin/runtime-organization-tests" (getenv "GERBIL_PATH"))
                      "-ld-options"
                      (string-append
                        (cond-expand (darwin "-Wl,-ld_classic ") (else ""))
                        (string-trim (run-process '("pkg-config" "--libs" "openssl"))))))
          srcdir: (current-directory) parallelize: 4 verbose: 1)
    (displayln "RUNTIME-ORGANIZATION-TESTS-PREPARED")
    (force-output)))
