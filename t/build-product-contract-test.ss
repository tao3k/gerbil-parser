;;; -*- Gerbil -*-
;;; Boundary: package installation owns reusable library products only.

(import (only-in :std/misc/ports read-all-as-string)
        (only-in :std/srfi/13 string-contains)
        (only-in :std/test check run-tests! test-case test-suite))

(def build-product-contract-tests
  (test-suite "build product ownership"
    (test-case "default package build excludes the optional command"
      (let ((library-source
             (call-with-input-file "build.ss" read-all-as-string))
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string)))
        (check (and (string-contains library-source
                                     "\"build-gparse.ss\"")
                    (string-contains library-source
                                     "\"src/cli.ss\"")
                    #t)
               => #t)
        (check (string-contains library-source
                                "(exe: \"src/main\"")
               => #f)
        (check (and (string-contains command-source
                                     "defbuild-script")
                    (string-contains command-source
                                     "\"src/cli\"")
                    (string-contains command-source
                                     "(exe: \"src/main\" bin: \"gparse\")")
                    #t)
               => #t)
        (check (string-contains command-source
                                "asp-gerbil-scheme-package-spec!")
               => #f)))))

(run-tests! build-product-contract-tests)
