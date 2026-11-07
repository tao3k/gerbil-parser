;;; Independent changing-input oracle preparation, never timed parser work.
(import :std/misc/ports
        :gerbil-parser/src/compiler/event-fold-runtime
        :gerbil-parser/t/fold-tape-oracle
        :orgize/languages/org/grammar)
(export main)

(def (main fixtures program-path digest output)
  (let* ((program (call-with-input-file program-path read))
         (sources (list->vector
                   (map (lambda (name)
                          (call-with-input-file (path-expand name fixtures) read-all-as-string))
                        '("doc.org" "plain-links.org" "quote-heavy.org"))))
         (inputs (list->vector
                  (map (lambda (index)
                         (string-append (vector-ref sources (modulo index 3))
                                        "\nchanging request " (number->string index) " α中🦀\n"))
                       (iota 1000 1000))))
         (expected (vector-map
                    (lambda (source)
                      (canonical-fold-tape org-mode-language-grammar
                                           (apply run-event-fold source program)))
                    inputs)))
    (call-with-output-file output
      (lambda (port)
        (write (list 'real-fold-preflight.v1 digest inputs expected) port)
        (newline port)))
    (displayln "PREPARATION-OK changing-inputs=1000 input-index-base=1000")))
