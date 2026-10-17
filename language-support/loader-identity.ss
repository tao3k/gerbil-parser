;;; -*- Gerbil -*-
;;; Private common identity utilities shared by public and development entries.
(import (only-in ../src/language/entry parse-language-source call-with-language-parser-policy)
        (only-in :clan/poo/object .o .ref .cc .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../src/compiler/machine parser-machine-grammar-digest)
        (only-in ../src/language/descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version language-grammar-parser-policy
                 language-parser-policy-branch-budget language-parser-policy-cst-validator language-grammar-with-identity)
        (only-in ../src/language/source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest source-language-scanner-factory source-language-with-identity))
(export +language-parser-entry-schema+ language-parser-entry-ref descriptor-capabilities
        require-grammar-descriptor require-source-descriptor descriptor-ref descriptor-digest
        descriptor-digest-kind bind-loader-metadata loader-metadata? language-metadata-ref bind-parser-identity)

(def +language-parser-entry-schema+ "gerbil-parser.language-entry.v2")

(def (language-parser-entry-ref entry key)
  (and (object? entry) (.slot? entry key) (.ref entry key)))

(def (descriptor-capabilities descriptor)
  (cond ((source-language? descriptor) '(scheme-source))
        ((language-grammar-parser-policy descriptor)
         '(grammar-ir parser-ir scheme scheme-policy))
        (else '(grammar-ir parser-ir scheme))))

(def (require-grammar-descriptor descriptor)
  (unless (language-grammar? descriptor)
    (error "grammar loader requires a generated language descriptor" descriptor))
  descriptor)

(def (require-source-descriptor descriptor)
  (unless (source-language? descriptor)
    (error "source loader requires a source language descriptor" descriptor))
  descriptor)

(def (descriptor-ref descriptor field)
  (if (source-language? descriptor)
    (case field
      ((language) (source-language-language descriptor))
      ((version) (source-language-version descriptor))
      ((contract) (source-language-contract descriptor)))
    (case field
      ((language) (language-grammar-language descriptor))
      ((version) (language-grammar-version descriptor))
      ((contract) (language-grammar-contract descriptor)))))

(def (descriptor-digest descriptor)
  (if (source-language? descriptor) (source-language-digest descriptor)
    (parser-machine-grammar-digest (language-grammar-machine descriptor))))

(def (descriptor-digest-kind descriptor)
  (if (source-language? descriptor) 'source-identity 'parser-ir))

;;; Metadata admits native POO objects or ordinary association-list values.
;;; POO inheritance remains native; value users do not construct an object.
(def (metadata-value? value)
  (and (list? value)
       (let loop ((rows value) (seen '()))
         (or (null? rows)
             (and (pair? (car rows)) (symbol? (caar rows))
                  (not (memq (caar rows) seen))
                  (loop (cdr rows) (cons (caar rows) seen)))))))
(def (require-metadata value)
  (unless (or (object? value) (metadata-value? value))
    (error "metadata requires a POO object or distinct symbol-keyed association list" value))
  value)
(def (language-metadata-ref value key)
  (require-metadata value)
  (if (object? value)
    (and (.slot? value key) (.ref value key))
    (let (row (assq key value)) (and row (cdr row)))))
(def (bind-parser-identity definition metadata)
  (require-metadata metadata)
  (if (and (descriptor-ref definition 'language)
           (descriptor-ref definition 'version) (descriptor-ref definition 'contract))
    definition
    (let ((language-value (language-metadata-ref metadata 'language))
          (version-value (language-metadata-ref metadata 'version))
          (contract-value (language-metadata-ref metadata 'contract)))
      (unless (andmap (lambda (value) (and (string? value) (positive? (string-length value))))
                     (list language-value version-value contract-value))
        (error "parser metadata requires nonempty language, version and contract"))
      (if (source-language? definition)
        (source-language-with-identity definition language-value version-value contract-value)
        (language-grammar-with-identity definition language-value version-value contract-value)))))
(def (bind-loader-metadata descriptor inherited)
  (require-metadata inherited)
  (let ((language-value (descriptor-ref descriptor 'language))
        (version-value (descriptor-ref descriptor 'version))
        (contract-value (descriptor-ref descriptor 'contract))
        (digest-value (descriptor-digest descriptor))
        (digest-kind-value (descriptor-digest-kind descriptor)))
    (if (object? inherited)
      (.o (:: self inherited)
          language: language-value version: version-value contract: contract-value
          digest: digest-value digest-kind: digest-kind-value)
      (append (list (cons 'language language-value) (cons 'version version-value)
                    (cons 'contract contract-value) (cons 'digest digest-value)
                    (cons 'digest-kind digest-kind-value))
              (filter (lambda (row) (not (memq (car row) '(language version contract digest digest-kind)))) inherited)))))
(def (loader-metadata? metadata descriptor)
  (and (or (object? metadata) (metadata-value? metadata))
       (andmap (lambda (key)
                 (equal? (language-metadata-ref metadata key) (descriptor-ref descriptor key)))
               '(language version contract))
       (equal? (language-metadata-ref metadata 'digest) (descriptor-digest descriptor))
       (eq? (language-metadata-ref metadata 'digest-kind) (descriptor-digest-kind descriptor))))
