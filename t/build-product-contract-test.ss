;;; -*- Gerbil -*-
;;; Boundary: package installation owns reusable library products only.

(import (only-in :std/misc/ports read-all-as-string)
        (only-in :std/test check test-case test-suite)
        (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-package-native-capabilities)
        (only-in :gerbil-parser/src/build-support/rust-rowan-aot
                 rust-rowan-aot-runtime-modules
                 rust-rowan-aot-package
                 rust-rowan-aot-build-spec))

(def (native-item-kind? kind item)
  (and (pair? item) (eq? (car item) kind)))

(def build-product-contract-tests
  (test-suite "build product ownership"
    (test-case "default package build excludes optional commands"
      (let ((library-source
             (call-with-input-file "build.ss" read-all-as-string))
            (package-source
             (call-with-input-file "gerbil.pkg" read-all-as-string))
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string)))
        (check (and (string-contains library-source
                                     "\"build-gparse.ss\"")
                    (string-contains library-source
                                     "\"build-rust-rowan-aot.ss\"")
                    (string-contains library-source
                                     ":asp-gerbil-scheme/building-api")
                    (string-contains library-source
                                     "\"src/cli.ss\"")
                    (string-contains library-source
                                     "\"src/ffi/rust-rowan-aot-main.ss\"")
                    #t)
               => #t)
        (check (and (string-contains package-source
                                     "poo-flow@a321c63")
                    (not (string-contains package-source
                                          "asp-gerbil-scheme@")))
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
               => #f)))
    (test-case "ASP Building API projects the Rowan executable closure"
      (let* ((spec (rust-rowan-aot-build-spec))
             (gxc-items
              (filter (lambda (item) (native-item-kind? 'gxc: item)) spec))
             (exe-items
              (filter (lambda (item) (native-item-kind? 'exe: item)) spec)))
        (check (map cadr gxc-items) => rust-rowan-aot-runtime-modules)
        (check (length exe-items) => 1)
        (let (executable (car exe-items))
          (check (cadr executable) => "src/ffi/rust-rowan-aot-main")
          (check (cadr (member 'bin: executable))
                 => "gerbil-parser-rowan-aot")
          (check (and (member "-cc-options" executable)
                      (member "-ld-options" executable)
                      #t)
                 => #t))
        (check (asp-gerbil-scheme-package-native-capabilities
                rust-rowan-aot-package)
               => '(tls))))))

(export build-product-contract-tests)
