;;; -*- Gerbil -*-
;;; Standalone build-time frontend for the Scheme-owned Rust generator.

(import (only-in :gerbil/runtime gerbil-load-expander!)
        (only-in :gerbil/runtime/loader set-load-path!)
        (only-in :gerbil-parser/rust-runtime-grammar-support deflanguage)
        (for-syntax :gerbil-parser/rust-runtime-grammar-support)
        (only-in ./rust-runtime-aot rust-runtime-source))
(export main)

(def (write-generated-source output-path source)
  (call-with-output-file output-path
    (lambda (port) (display source port))))

(def (main . arguments)
  (unless (= (length arguments) 2)
    (display "usage: gerbil-parser-runtime-aot GRAMMAR.ss OUTPUT.rs"
             (current-error-port))
    (newline (current-error-port))
    (exit 64))
  (cond
   ((getenv "GERBIL_PARSER_RUNTIME_AOT_LIB" #f)
    => (lambda (paths) (set-load-path! (string-split paths #\:)))))
  (gerbil-load-expander!)
  (write-generated-source
   (cadr arguments)
   (rust-runtime-source (car arguments))))
