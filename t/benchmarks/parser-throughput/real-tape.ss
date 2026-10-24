;;; Actual generated Org Fold directly consumed as the production OEV1 tape.
;;; Independent interpreter/list encoding is outside measured requests.
(import :std/misc/ports
        :gerbil-parser/src/compiler/event-fold-runtime
        :gerbil-parser/t/fold-tape-oracle
        :gerbil-parser/t/benchmarks/parser-throughput/protocol
        :orgize/languages/org/grammar
        :gerbil-parser/benchmarks/real-fold/parse-org-compiled-events)
(export main)
(def (main fixtures program-path digest (mode "preflight") (consumer "direct"))
  (unless (member consumer '("direct" "list-control")) (error "invalid Fold consumer lane"))
  (unless (eq? (bind-parse-org-compiled-events digest) parse-org-compiled-events)
    (error "direct real Fold binding mismatch"))
  (let ((program (call-with-input-file program-path read))
        (legacy-consumer (make-legacy-fold-tape-consumer org-mode-language-grammar))
        (sources (list->vector
                  (map (lambda (name)
                         (call-with-input-file (path-expand name fixtures) read-all-as-string))
                       '("doc.org" "plain-links.org" "quote-heavy.org")))))
    (run-parser-throughput (string-append "upstream-real-org-fold-oev1-" consumer) digest
      (if (equal? consumer "direct") parse-org-compiled-events
        (lambda (source) (legacy-consumer (parse-org-compiled-events source))))
      (lambda (source) (canonical-fold-tape org-mode-language-grammar
                        (apply run-event-fold source program)))
      (lambda (index)
        (string-append (vector-ref sources (modulo index 3))
                       "\nchanging request " (number->string index) " α中🦀\n"))
      (string->symbol mode))))
