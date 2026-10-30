;;; Immutable source identity shared by metadata, declarations and requests.
;;; No strategy binding, profile compilation or parser preparation occurs here.
(import (only-in ../runtime/identity sha256-text)
        (only-in ../runtime/result-plan result-plan-catalog))
(export +source-language-schema+ make-source-language source-language?
        source-language-schema source-language-language source-language-version
        source-language-contract source-language-digest source-language-scanner
        source-language-parse source-language-factory source-language-receipt
        source-language-results source-language-root-kind source-language-program
        make-source-syntax source-syntax? source-syntax-recipe source-syntax-strategy
        source-language-with-identity source-language-scanner-factory source-language-result-catalog)

(def +source-language-schema+ "gerbil-parser.source-language.v1")

(defstruct source-language
  (schema language version contract digest scanner parse factory receipt results root-kind program)
  transparent: #t)

(def (source-language-scanner-factory descriptor) (source-language-factory descriptor))

(defstruct (source-syntax source-language) (recipe strategy) transparent: #t)
(def (source-language-with-identity definition language version contract)
  (unless (source-syntax? definition) (error "source identity requires unbound syntax"))
  (let ((language-value (string-copy language)) (version-value (string-copy version))
        (contract-value (string-copy contract)))
    (make-source-language +source-language-schema+ language-value version-value contract-value
      (sha256-text (call-with-output-string
        (lambda (port) (write (list language-value version-value contract-value
                                   (source-syntax-recipe definition)) port))))
      (source-language-scanner definition) (source-language-parse definition)
      (source-language-factory definition) (source-language-receipt definition)
      (source-language-results definition) (source-language-root-kind definition)
      (source-language-program definition))))


(def (source-language-result-catalog descriptor)
  (result-plan-catalog (source-language-results descriptor)))
