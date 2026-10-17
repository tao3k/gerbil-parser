;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        (only-in :gerbil-parser/language-source-support deflanguage-parser-receipt)
        (only-in :gerbil-parser/src/runtime/shell-parser
                 shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)
        ./grammar)
(export +bash-version+ +bash-syntax-contract+ bash-source-language (import: ./grammar) bash-language parse-bash parse-bash/receipt
        shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)

(deflanguage-parser-loader bash-language
  (source bash-syntax)
  (parse parse-bash)
  (slots metadata: '((language . "bash")
              (version . "5.3")
              (contract . "bash-5.3-structured-source.v1")
              (grammar-format . source-parser))))

(def bash-source-language (language-parser-entry-ref bash-language 'descriptor))

(deflanguage-parser-receipt parse-bash/receipt bash-source-language)

(def +bash-version+ (language-metadata-ref (language-parser-entry-ref bash-language 'metadata) 'version))

(def +bash-syntax-contract+ (language-metadata-ref (language-parser-entry-ref bash-language 'metadata) 'contract))
