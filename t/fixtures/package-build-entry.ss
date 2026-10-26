;;; Fixture for the build declaration's spec API, not a child build/test entry.
(import (only-in :gerbil/expander import-module current-expander-context eval-syntax))
(export query-spec)

(def (query-spec root entry (noise? #f))
  (let (context (import-module (path-expand entry root) #f #t))
    (parameterize ((current-expander-context context))
      (let ((original (eval-syntax 'modules-spec))
            (replace! (eval-syntax '(lambda (value) (set! modules-spec value)))))
        (try
          (when noise?
            (replace! (lambda (roots)
                        (displayln "SPEC-PREPARATION-NOISE")
                        (original roots))))
          (let (output
                 (call-with-output-string
                   (lambda (port)
                     (parameterize ((current-output-port port))
                       (eval-syntax '(main "spec"))))))
            (call-with-input-string output
              (lambda (input)
                (let (spec (read input))
                  (unless (and (list? spec) (eof-object? (read input)))
                    (error "native build spec did not publish one list datum" entry))
                  spec))))
          (finally (replace! original)))))))
