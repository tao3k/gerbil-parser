#!/usr/bin/env gxi
;;; Full language and endpoint witnesses are produced by the Scheme owner.
(import (only-in :gerbil-parser/src/compiler/rust-runtime language-rust-runtime-module-source)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-language-grammar)
        (only-in "text-profile-cases.ss" fhirpath-profile-cases))
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))
(def (main output)
  (call-with-output-file output
    (lambda (port)
      (display (language-rust-runtime-module-source fhirpath-language-grammar) port)
      (display "\npub const PROFILE_CASES: &[(&str, &str, Option<usize>)] = &[\n" port)
      (for-each
       (lambda (group)
         (for-each
          (lambda (row)
            (display "(" port) (write (symbol->string (car group)) port)
            (display ", " port) (write (car row) port) (display ", " port)
            (if (cadr row)
              (begin (display "Some(" port) (display (cadr row) port) (display ")" port))
              (display "None" port))
            (display "),\n" port))
          (cdr group)))
       fhirpath-profile-cases)
      (display "];\n" port)))
  (displayln "TEXT-PROFILE-RUST-GENERATED") (force-output) (test-child-process-exit! 0))
