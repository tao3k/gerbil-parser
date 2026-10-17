;;; -*- Gerbil -*-
;;; Boundary: package installation owns reusable library products only.

(import (only-in :clan/poo/object .o .get)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/test check test-case test-suite)
        (only-in :std/text/pregexp pregexp-match)
        (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-package-native-capabilities
                 asp-gerbil-scheme-portable-native-profile)
        (only-in :gerbil-parser/src/build-support/rust-runtime-aot
                 rust-runtime-aot-runtime-modules
                 rust-runtime-aot-package
                 rust-runtime-aot-build-spec))

(def (native-item-kind? kind item)
  (and (pair? item) (eq? (car item) kind)))

(def build-product-contract-tests
  (test-suite "build product ownership"
    (test-case "default package build excludes optional commands"
      (let* ((cargo-source (call-with-input-file "Cargo.toml" read-all-as-string))
             (bridge (pregexp-match "gerbil-scheme-rust-ir = [^\n]*rev = \"([0-9a-f]{40})\"" cargo-source))
             (library-source
             (call-with-input-file "build.ss" read-all-as-string))
            (package-source
             (call-with-input-file "gerbil.pkg" read-all-as-string))
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string)))
        (check (and (string-contains library-source
                                     "\"build-gparse.ss\"")
                    (string-contains library-source
                                     "\"build-rust-runtime-aot.ss\"")
                    (string-contains library-source
                                     ":asp-gerbil-scheme/building-api")
                    (string-contains library-source
                                     "\"src/cli.ss\"")
                    (string-contains library-source
                                     "\"src/ffi/rust-runtime-aot-main.ss\"")
                    #t)
               => #t)
        (check (and bridge #t) => #t)
        (check (cadr (member 'depend: (call-with-input-string package-source read)))
               => (list "github.com/tao3k/poo-flow-core@e85fd45303b208b23e5957fd316e7004994109a4"
                         "github.com/tao3k/asp-gerbil-scheme@f5b7c009d2cab61144a8af18008a0bfba3056584"))
        (check (string-contains library-source
                                "(exe: \"src/main\"")
               => #f)
        ;; A foreign context must be excluded from generic discovery and
        ;; included in the explicit FFI specs that carry Darwin bundle flags.
        (check (and (string-contains library-source "\"src/ffi/language-abi-context.ss\"")
                    (string-contains library-source "\"src/ffi/language-abi-context\"")
                    (string-contains library-source "-Wl,-undefined,dynamic_lookup")
                    #t)
               => #t)
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
    (test-case "ASP Building API projects the Rust generator executable closure"
      ;; This is the production POO projector with deterministic host slots.
      ;; Host discovery (including brew/pkg-config) belongs to the actual build,
      ;; not to a topology contract running under the per-owner watchdog.
      (let* ((prepared '()) (resolved 0)
             (package (.o (:: @ rust-runtime-aot-package)
                         native-profile: (.o (:: @ asp-gerbil-scheme-portable-native-profile)
                           prepare!: (lambda (libraries) (set! prepared (cons libraries prepared))))
                         native-options-resolver: (lambda () (set! resolved (+ resolved 1))
                                                   '("-cc-options" "-Icontract" "-ld-options" "-lcontract"))))
             (spec ((.get package spec-projector) package))
             (gxc-items
              (filter (lambda (item) (native-item-kind? 'gxc: item)) spec))
             (exe-items
              (filter (lambda (item) (native-item-kind? 'exe: item)) spec)))
        (check prepared => '(("openssl")))
        (check resolved => 1)
        (check (map cadr gxc-items) => rust-runtime-aot-runtime-modules)
        (check (length exe-items) => 1)
        (let (executable (car exe-items))
          (check (cadr executable) => "src/ffi/rust-runtime-aot-main")
          (check (cadr (member 'bin: executable))
                 => "gerbil-parser-runtime-aot")
          (check (and (member "-cc-options" executable)
                      (member "-ld-options" executable)
                      #t)
                 => #t)
          (check (cadr (member "-cc-options" executable)) => "-Icontract")
          (check (cadr (member "-ld-options" executable)) => "-lcontract"))
        (check (asp-gerbil-scheme-package-native-capabilities
                rust-runtime-aot-package)
               => '(tls))))))

(export build-product-contract-tests)

;; gxtest discovers only exported names ending in -test.
(def build-product-contract-test build-product-contract-tests)
(export build-product-contract-test)
