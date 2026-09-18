;;; -*- Gerbil -*-
;;; Standalone build-time frontend for the Scheme-owned Rust/Rowan generator.

(import (only-in :gerbil/runtime gerbil-load-expander!)
        (only-in :gerbil/runtime/loader set-load-path!)
        (only-in :std/srfi/13 string-split)
        (only-in :gerbil-parser/rust-rowan-grammar-support deflanguage)
        (for-syntax :gerbil-parser/rust-rowan-grammar-support)
        (only-in ./rust-rowan-aot-v1 native-rust-rowan-source))
(export main)

(def (write-generated-source output-path source)
  (call-with-output-file output-path
    (lambda (port) (display source port))))

(def (main . arguments)
  (unless (= (length arguments) 2)
    (display "usage: gerbil-parser-rowan-aot GRAMMAR.ss OUTPUT.rs"
             (current-error-port))
    (newline (current-error-port))
    (exit 64))
  (cond
   ((getenv "GERBIL_PARSER_ROWAN_AOT_LIB" #f)
    => (lambda (paths) (set-load-path! (string-split paths #\:)))))
  (gerbil-load-expander!)
  (write-generated-source
   (cadr arguments)
   (native-rust-rowan-source (car arguments))))
