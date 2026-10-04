;;; Actual Rust-owned handles and GPA1 buffers over the compiled language API.
(import :gerbil-parser/src/ffi/language-v2-native
        :gerbil-parser/t/fixtures/shared-scanner/records-native
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(export main)
(extern rust-native-probe)
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/native-ffi/rust-language-probe#" rust-native-probe))
 (c-declare "int gerbil_parser_rust_native_probe(void);")
 (define rust-native-probe (c-lambda () int "gerbil_parser_rust_native_probe")))
(def (main . _)
  (displayln "RUST-NATIVE-START") (force-output)
  (unless (zero? (rust-native-probe)) (error "Rust native ownership probe failed"))
  (displayln "RUST-NATIVE-OK") (force-output)
  (test-child-process-exit! 0))
