;;; Actual Rust-owned handles and GPA1 buffers over the compiled language API.
(import :gerbil-parser/src/ffi/language-native
        :gerbil-parser/t/fixtures/shared-scanner/records-native
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/src/ffi/language-handles register-native-language! native-language-handle-count)
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(export main)
(extern rust-native-probe rust-native-source-probe)
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/native-ffi/rust-language-probe#" rust-native-probe rust-native-source-probe))
 (c-declare "#include <stdint.h>\nint gerbil_parser_rust_native_probe(void);\nint gerbil_parser_rust_native_source_probe(uint64_t, uint64_t);")
 (define rust-native-probe (c-lambda () int "gerbil_parser_rust_native_probe"))
 (define rust-native-source-probe (c-lambda (unsigned-int64 unsigned-int64) int "gerbil_parser_rust_native_source_probe")))
(def (main . _)
  (displayln "RUST-NATIVE-START") (force-output)
  (unless (zero? (rust-native-probe)) (error "Rust native ownership probe failed"))
  (let* ((baseline (native-language-handle-count))
         (first (register-native-language! bash-source-language))
         (second (register-native-language! bash-source-language)))
    (unless (zero? (rust-native-source-probe first second)) (error "Rust Source edit probe failed"))
    (unless (= baseline (native-language-handle-count)) (error "Rust Source handle leak")))
  (displayln "RUST-NATIVE-OK") (force-output)
  (test-child-process-exit! 0))
