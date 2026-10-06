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
                 language-parser-policy-branch-budget language-parser-policy-cst-validator)
        (only-in ../src/language/source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest source-language-scanner-factory))
(export +language-parser-entry-schema+ language-parser-entry-ref descriptor-capabilities
        require-grammar-descriptor require-source-descriptor descriptor-ref descriptor-digest
        descriptor-digest-kind bind-loader-metadata loader-metadata?)

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

;;; Metadata extends ordinary POO values; identity is derived and sealed here.
(def (bind-loader-metadata descriptor inherited)
  (.o (:: self inherited)
      language: (descriptor-ref descriptor 'language)
      version: (descriptor-ref descriptor 'version)
      contract: (descriptor-ref descriptor 'contract)
      digest: (descriptor-digest descriptor)
      digest-kind: (descriptor-digest-kind descriptor)))

(def (loader-metadata? metadata descriptor)
  (and (object? metadata)
       (andmap (lambda (key) (.slot? metadata key))
               '(language version contract digest digest-kind))
       (andmap (lambda (key)
                 (equal? (.ref metadata key) (descriptor-ref descriptor key)))
               '(language version contract))
       (equal? (.ref metadata 'digest) (descriptor-digest descriptor))
       (eq? (.ref metadata 'digest-kind) (descriptor-digest-kind descriptor))))
