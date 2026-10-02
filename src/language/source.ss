;;; -*- Gerbil -*-
;;; Versioned language ownership for stateful source parsers. The scanner and
;;; parser are one immutable declaration and must publish a valid artifact.

(import (only-in ../runtime/artifact
                 parse-artifact-ref parse-artifact-valid? sha256-text))
(export +source-language-schema+
        declare-source-language
        source-language?
        source-language-language
        source-language-version
        source-language-contract
        source-language-digest
        parse-source-language)

(def +source-language-schema+ "gerbil-parser.source-language.v1")

(defstruct source-language
  (schema language version contract digest scanner parse)
  transparent: #t)

(def (declare-source-language language version contract scanner parse)
  (unless (and (string? language) (string? version)
               (string? contract) (procedure? scanner)
               (procedure? parse))
    (error "invalid source language declaration"))
  (make-source-language
   +source-language-schema+ language version contract
   (sha256-text (string-append language "/" version "/" contract))
   scanner parse))

(def (parse-source-language descriptor source)
  (unless (string? source)
    (error "source language requires a string" source))
  (let* ((digest (source-language-digest descriptor))
         (artifact
          ((source-language-parse descriptor)
           source (source-language-scanner descriptor) digest)))
    (unless (and (parse-artifact-valid? artifact)
                 (equal? (parse-artifact-ref artifact 'grammarDigest)
                         digest)
                 (equal? (parse-artifact-ref artifact 'sourceDigest)
                         (sha256-text source)))
      (error "source parser published an invalid artifact"))
    artifact))
