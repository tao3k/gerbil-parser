(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Boundary: package installation owns reusable library products only.

(import (only-in :clan/poo/object .o .get)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/test check test-case test-suite)
        (only-in :std/text/pregexp pregexp-match)
        (only-in :asp-gerbil-scheme/building-api
                 asp-gerbil-scheme-package-native-capabilities
                 asp-gerbil-scheme-portable-native-profile)
        (only-in "../dev/rust-runtime-aot"
                 rust-runtime-aot-runtime-modules
                 rust-runtime-aot-package
                 rust-runtime-aot-build-spec))

(def (native-item-kind? kind item)
  (and (pair? item) (eq? (car item) kind)))

(deflanguage-parser-tests build-product-contract-tests "build product ownership"
  (engine)
    (scenario "default package build excludes optional commands"
      (sequential ((cargo-source (call-with-input-file "Cargo.toml" read-all-as-string))
             (bridge (pregexp-match "gerbil-scheme-rust-ir = [^\n]*rev = \"([0-9a-f]{40})\"" cargo-source))
             (library-source
             (call-with-input-file "dev/package-source-build.ss" read-all-as-string))
            (package-source
             (call-with-input-file "gerbil.pkg" read-all-as-string))
            (command-source
             (call-with-input-file "build-gparse.ss" read-all-as-string)))
        (equal (and (string-contains library-source
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
                #t)
        (equal (and bridge #t)  #t)
        ;; The package owns revision selection. Check immutable public owners,
        ;; rather than duplicating a historical dependency pin in this test.
        (equal (map (lambda (dependency)
                      (let (match (pregexp-match
                                    "^github\\.com/tao3k/(gebril-yuanxu)@[0-9a-f]{40}$"
                                    dependency))
                        (and match (cadr match))))
                    (cadr (member 'depend: (call-with-input-string package-source read))))
               '("gebril-yuanxu"))
        (equal (string-contains
                 (call-with-input-file "build.ss" read-all-as-string)
                 "asp-gerbil-scheme") #f)
        (equal (map (lambda (dependency)
                      (let (match (pregexp-match
                                    "^github\\.com/tao3k/(asp-gerbil-scheme)@[0-9a-f]{40}$"
                                    dependency))
                        (and match (cadr match))))
                    (cadr (member 'depend:
                           (call-with-input-file "dev/gerbil.pkg" read))))
               '("asp-gerbil-scheme"))
        (equal (string-contains library-source
                                "(exe: \"src/main\"")
                #f)
        ;; A foreign context must be excluded from generic discovery and
        ;; included in the explicit FFI specs that carry Darwin bundle flags.
        (equal (and (string-contains library-source "\"src/ffi/language-abi-context.ss\"")
                    (string-contains library-source "\"src/ffi/language-abi-context\"")
                    (string-contains library-source "-Wl,-undefined,dynamic_lookup")
                    #t)
                #t)
        ;; The native compiler/CLI integration verifies executable construction.
        ;; Do not freeze a textual spelling that rejects additional link inputs.
        (equal (string-contains command-source
                                "asp-gerbil-scheme-package-spec!")
                #f)))
    (scenario "ASP Building API projects the Rust generator executable closure"
      ;; This is the production POO projector with deterministic host slots.
      ;; Host discovery (including brew/pkg-config) belongs to the actual build,
      ;; not to a topology contract running under the per-owner watchdog.
      (sequential ((prepared '()) (resolved 0)
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
        (equal prepared  '(("openssl")))
        (equal resolved  1)
        (equal (map cadr gxc-items)  rust-runtime-aot-runtime-modules)
        (equal (length exe-items)  1)
        (scope ((executable (car exe-items)))
          (equal (cadr executable)  "src/ffi/rust-runtime-aot-main")
          (equal (cadr (member 'bin: executable))
                  "gerbil-parser-runtime-aot")
          (equal (and (member "-cc-options" executable)
                      (member "-ld-options" executable)
                      #t)
                  #t)
          (equal (cadr (member "-cc-options" executable))  "-Icontract")
          (equal (cadr (member "-ld-options" executable))  "-lcontract"))
        (equal (asp-gerbil-scheme-package-native-capabilities
                rust-runtime-aot-package)
                '(tls)))))


;; gxtest discovers only exported names ending in -test.
(def build-product-contract-test build-product-contract-tests)
(export build-product-contract-test)
