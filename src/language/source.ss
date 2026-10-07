;;; -*- Gerbil -*-
;;; Versioned language ownership for stateful source parsers. The scanner and
;;; parser are one immutable declaration and must publish a valid artifact.

(import (only-in ../runtime/artifact
                 parse-artifact-ref parse-artifact-valid? sha256-text)
        (only-in ./source-strategy bind-source-strategy source-engine-scanner source-engine-factory
                 source-engine-parse source-engine-receipt source-engine-results)
        (only-in ./result-profile result-plan-catalog))
(export +source-language-schema+
        declare-source-language declare-source-syntax source-language-with-identity source-syntax-strategy
        source-language?
        source-language-language
        source-language-version
        source-language-contract
        source-language-digest source-language-result-catalog
        source-language-scanner-factory parse-source-language parse-source-language/receipt
        deflanguage-parser-receipt)

(def +source-language-schema+ "gerbil-parser.source-language.v1")

(defstruct source-language
  (schema language version contract digest scanner parse factory receipt results)
  transparent: #t)

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
     (source-engine-factory engine) (source-engine-receipt engine) (source-engine-results engine)))))
(def (source-language-result-catalog descriptor)
  (result-plan-catalog (source-language-results descriptor)))
(def (source-language-scanner-factory descriptor) (source-language-factory descriptor))
(def (validate-source-publication! descriptor source artifact)
  (unless (and (parse-artifact-valid? artifact)
               (equal? (parse-artifact-ref artifact 'grammarDigest) (source-language-digest descriptor))
               (equal? (parse-artifact-ref artifact 'sourceDigest) (sha256-text source)))
    (error "source parser published an invalid artifact"))
  artifact)
(def (parse-source-language/receipt descriptor source)
  (unless (and (string? source) (source-language? descriptor) (procedure? (source-language-receipt descriptor)))
    (error "source engine does not declare receipts for this input"))
  (let-values (((artifact receipt)
                ((source-language-receipt descriptor) source (source-language-scanner descriptor)
                 (source-language-digest descriptor))))
    (validate-source-publication! descriptor source artifact)
    (values artifact receipt)))

(defrules deflanguage-parser-receipt ()
  ((_ binding descriptor)
   (def (binding source) (parse-source-language/receipt descriptor source))))

(def (parse-source-language descriptor source)
  (unless (string? source)
    (error "source language requires a string" source))
  (let* ((digest (source-language-digest descriptor))
         (artifact
          ((source-language-parse descriptor)
           source (source-language-scanner descriptor) digest)))
    (validate-source-publication! descriptor source artifact)))

;;; An internal unbound syntax product; parser metadata supplies release identity.
(defstruct (source-syntax source-language) (recipe strategy) transparent: #t)
(def (declare-source-syntax strategy)
  (let-values (((recipe engine) (bind-source-strategy strategy)))
    (make-source-syntax +source-language-schema+ #f #f #f #f
      (source-engine-scanner engine) (source-engine-parse engine)
      (source-engine-factory engine) (source-engine-receipt engine)
      (source-engine-results engine) recipe strategy)))
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
      (source-language-results definition))))
