;;; Application-specific C export; VM ownership belongs to gerbil-scheme-rust.
(import :gerbil-scheme-rust/scheme/runtime
        (only-in :gerbil-parser/t/fixtures/ffi/rust-language-probe verify-rust-ownership!))
(export verify-program! main)
(def (main . _) (void))
(def (verify-program!)
  (with-exception-catcher
    (lambda (error)
      (display-exception error (current-error-port))
      (force-output (current-error-port))
      1)
    (lambda () (verify-rust-ownership!) 0)))
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/rust-language-abi/program#" verify-callback))
 (c-define (verify-callback) () int "gerbil_parser_verify_rust_ownership" "extern"
   (gerbil-parser/t/fixtures/rust-language-abi/program#verify-program!)))
