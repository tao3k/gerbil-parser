;;; Native engine-line-fold adapter, explicitly not the complete Org parser.
(import (only-in :gerbil-parser/t/benchmarks/parser-throughput/protocol
                 run-parser-throughput)
        (only-in :gerbil-parser/src/runtime/source-lines source-line-events))
(export main)

(def (input index)
  (string-append "heading " (number->string index) "\r\n"
                 "changing α中🦀 payload\nlast " (number->string index)))

(def (oracle source)
  (source-line-events source 'Document
    (lambda (_ from until)
      (list '(start Text) (list 'token 'Line from until) '(finish)))))

(def (main (mode "preflight"))
  (run-parser-throughput "engine-line-fold-not-org" fixture-identity
                        throughput-fold oracle input (string->symbol mode)))
