;;; Build-only engine fixture; never substitute this grammar for Org acceptance.
(import (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/t/event-strategy-fixture event-lines-language-grammar)
        (only-in :gerbil-parser/src/compiler/event-fold-ir event-fold-ir)
        (only-in :gerbil-parser/src/compiler/event-fold-scheme event-fold-scheme-source))
(export main)

(def (main output)
  (let* ((forms '((start-node Text) (token Line start end) (finish-node)))
         (identity (hash-get (event-fold-ir 'throughput-fold event-lines-language-grammar
                                           'Document '() forms '()) "parser_digest")))
    (call-with-output-file output
      (lambda (port)
        (display (event-fold-scheme-source 'throughput-fold event-lines-language-grammar
                                          'Document '() forms '()) port)
        (write `(def fixture-identity ,identity) port) (newline port)
        (display (call-with-input-file "t/benchmarks/parser-throughput/body.ss"
                                       read-all-as-string) port)))))
