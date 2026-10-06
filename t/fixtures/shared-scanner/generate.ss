#!/usr/bin/env gxi
;;; Compile Scanner IR and Scheme-executed token traces for Rust conformance.
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)
        "declaration"
        (only-in :gerbil-parser/src/compiler/rust-scanner rust-scanner-module-source)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 prepare-contextual-scanner contextual-scanner-initial-state contextual-scanner-step)
        (only-in :gerbil-parser/src/runtime/token token-kind token-start token-end))
(def (main output)
  (let ((generated (rust-scanner-module-source shared-scanner-ir))
        (traces
         (map
          (lambda (source)
            (let (scanner (prepare-contextual-scanner shared-scanner-ir source))
              (let loop ((state (contextual-scanner-initial-state scanner)) (rows '()))
                (let-values (((token next) (contextual-scanner-step scanner state 'command)))
                  (if token
                    (loop next (cons (list (symbol->string (token-kind token)) (token-start token) (token-end token)) rows))
                    (cons source (reverse rows))))))) shared-scanner-sources)))
    (call-with-output-file output
      (lambda (port)
        (display generated port)
        (display "pub static TRACES: &[(&str, &[gerbil_parser_rowan::ScannedToken])] = &[\n" port)
        (for-each
         (lambda (trace)
           (display "(" port) (write (car trace) port) (display ", &[" port)
           (for-each (lambda (row)
                       (display "gerbil_parser_rowan::ScannedToken { terminal: " port)
                       (write (car row) port) (display ", start: " port) (display (cadr row) port)
                       (display ", end: " port) (display (caddr row) port) (display " }," port)) (cdr trace))
           (display "]),\n" port)) traces)
        (display "];\n" port)))
    (displayln "SHARED-SCANNER-GENERATED cases=" (length traces))
    (force-output) (test-child-process-exit! 0)))
