;;; Actual generated Fold; all runtime, measurement and oracle ownership is upstream.
(import :std/misc/ports
        :gerbil-parser/src/compiler/event-fold-runtime
        :gerbil-parser/t/benchmarks/parser-throughput/protocol
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)
(def (main fixtures program-path digest (mode "preflight"))
  (unless (eq? (bind-parse-org-compiled-events digest) parse-org-compiled-events)
    (error "real Fold binding mismatch"))
  (let ((program (call-with-input-file program-path read))
        (sources (list->vector
                  (map (lambda (name)
                         (call-with-input-file (path-expand name fixtures) read-all-as-string))
                       '("doc.org" "plain-links.org" "quote-heavy.org")))))
    (run-parser-throughput "upstream-real-org-fold" digest parse-org-compiled-events
      (lambda (source) (apply run-event-fold source program))
      (lambda (index)
        (string-append (vector-ref sources (modulo index 3))
                       "\nchanging request " (number->string index) " α中🦀\n"))
      (string->symbol mode))))
