;;; Canonical Rust outputs from two independent Scheme language declarations.
(import (only-in :gerbil-parser/language-build-support
                 make-rust-rowan-strategy declare-language-build-strategy emit-language-build-strategy)
        (only-in :clan/poo/object .cc .ref)
        (only-in :gerbil-parser/languages/hl7/parser hl7-language)
        (only-in "header-delimiter-test.ss" header-record-language))
(export main)
(def (emit loader output)
  (let (bound (.cc loader 'build-strategies
                  (list (declare-language-build-strategy 'rust-rowan
                          (make-rust-rowan-strategy (.ref loader 'descriptor))))))
    (call-with-output-file output
      (lambda (port) (emit-language-build-strategy bound 'rust-rowan port)))))
(def (main hl7-output record-output)
  (emit hl7-language hl7-output)
  (displayln "HEADER-DELIMITER-HL7-GENERATED") (force-output)
  (emit header-record-language record-output)
  (displayln "HEADER-DELIMITER-RECORD-GENERATED") (force-output))
