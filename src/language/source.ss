;;; Source declaration admission owns strategy and profile construction.
;;; Identity and request consumers import their narrower semantic owners.
(import ./source-descriptor ./source-parser
        (only-in ../runtime/identity sha256-text)
        (only-in ../compiler/contextual-program contextual-program-ir)
        (only-in ./source-strategy bind-source-strategy source-engine-scanner source-engine-factory
                 source-engine-parse source-engine-receipt source-engine-results source-engine-root-kind source-engine-program))
(export +source-language-schema+
        declare-source-language declare-source-syntax source-language-with-identity source-syntax-strategy
        source-language?
        source-language-language
        source-language-version
        source-language-contract
        source-language-digest source-language-result-catalog source-language-root-kind source-language-contextual-ir
        source-language-scanner-factory parse-source-language parse-source-language/receipt
        deflanguage-parser-receipt
        parse-source-language/session source-language-session? source-language-session-artifact
        source-language-history? source-language-session-history
        source-language-session-scanned-token-count source-language-session-reused-token-count)

(def (declare-source-language language version contract strategy)
  (unless (and (string? language) (positive? (string-length language))
               (string? version) (positive? (string-length version))
               (string? contract) (positive? (string-length contract)))
    (error "invalid source language identity"))
  (let ((language (string-copy language)) (version (string-copy version))
        (contract (string-copy contract)))
   (let-values (((recipe engine) (bind-source-strategy strategy)))
    (make-source-language
     +source-language-schema+ language version contract
     (sha256-text (call-with-output-string
                    (lambda (port) (write (list language version contract recipe) port))))
     (source-engine-scanner engine) (source-engine-parse engine)
     (source-engine-factory engine) (source-engine-receipt engine) (source-engine-results engine) (source-engine-root-kind engine) (source-engine-program engine)))))
(def (source-language-contextual-ir descriptor)
  (contextual-program-ir (source-language-program descriptor)))
(def (declare-source-syntax strategy)
  (let-values (((recipe engine) (bind-source-strategy strategy)))
    (make-source-syntax +source-language-schema+ #f #f #f #f
      (source-engine-scanner engine) (source-engine-parse engine)
      (source-engine-factory engine) (source-engine-receipt engine)
      (source-engine-results engine) (source-engine-root-kind engine) (source-engine-program engine) recipe strategy)))
