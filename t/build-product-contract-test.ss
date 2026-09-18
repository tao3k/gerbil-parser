;;; -*- Gerbil -*-
;;; Boundary: package installation owns reusable library products only.

(import (only-in :std/misc/ports read-all-as-string)
        (only-in :std/srfi/13 string-contains)
        (only-in :std/test check run-tests! test-case test-suite))

(def build-product-contract-tests
  (test-suite "build product ownership"
    (test-case "default package build excludes optional commands"
      (let ((library-source
             (call-with-input-file "build.ss" read-all-as-string))
            (package-source
             (call-with-input-file "gerbil.pkg" read-all-as-string))
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string))
            (rowan-aot-source
             (call-with-input-file
              "build-rust-rowan-aot.ss" read-all-as-string)))
        (check (and (string-contains library-source
                                     "\"build-gparse.ss\"")
                    (string-contains library-source
                                     "\"build-rust-rowan-aot.ss\"")
                    (string-contains library-source
                                     "\"src/cli.ss\"")
                    (string-contains library-source
                                     "\"src/ffi/rust-rowan-aot-main.ss\"")
                    #t)
               => #t)
        (check (string-contains package-source
                                "asp-gerbil-scheme@")
               => #f)
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
               => #f)
        (check (and (string-contains rowan-aot-source
                                     "defbuild-script")
                    (string-contains rowan-aot-source
                                     "src/ffi/rust-rowan-aot-v1")
                    (string-contains rowan-aot-source
                                     "src/ffi/rust-rowan-aot-main")
                    (string-contains rowan-aot-source
                                     ":asp-gerbil-scheme/build-api")
                    (string-contains rowan-aot-source
                                     "asp-gerbil-scheme-package-spec!")
                    (string-contains rowan-aot-source
                                     "(gxc: ,module)")
                    (string-contains rowan-aot-source
                                     "\"gerbil-parser-rowan-aot\"")
                    (string-contains rowan-aot-source
                                     "(role 'build-support)")
                    (string-contains rowan-aot-source
                                     "(native-capabilities '(tls))")
                    (string-contains rowan-aot-source
                                     "pkg-config-options")
                    #t)
               => #t)))))

(run-tests! build-product-contract-tests)
