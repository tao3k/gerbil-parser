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
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string))
            (rowan-aot-source
             (call-with-input-file
              "build-rust-rowan-aot.ss" read-all-as-string))
            (rowan-aot-bundle-source
             (call-with-input-file
              "build-support/materialize-rust-rowan-aot-bundle.sh"
              read-all-as-string)))
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
                                     "bin: \"gerbil-parser-rowan-aot\"")
                    #t)
               => #t)
        (check (and (string-contains rowan-aot-bundle-source
                                     "GERBIL_PARSER_ROWAN_AOT_TRACE_MODULES=1")
                    (string-contains rowan-aot-bundle-source
                                     "gsc -dynamic -o")
                    (string-contains rowan-aot-bundle-source
                                     "gerbil-parser.rowan-aot-bundle.v1")
                    (string-contains rowan-aot-bundle-source
                                     "@executable_path/../lib/native")
                    (string-contains rowan-aot-bundle-source
                                     "codesign --force --sign -")
                    (string-contains rowan-aot-bundle-source
                                     "rowan-aot-native-libraries.v1")
                    #t)
               => #t)))))

(run-tests! build-product-contract-tests)
