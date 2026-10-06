;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.
                 declare-language-source-scan-worker)
        (only-in :gerbil-parser/language-source-support deflanguage-source-receipt)
        (only-in :gerbil-parser/src/runtime/shell-parser
                 shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)
        ./grammar)
(export (import: ./grammar) bash-language parse-bash parse-bash/receipt
        shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)

(deflanguage-development-loader (bash-language :: self LanguageDevelopmentLoader.)
  (source bash-source-language)
  (parse parse-bash)
  (slots metadata: (.o grammar-format: 'source-parser)
         fixtures: bash-fixtures
         scan-workers: (list (cons 'command
                                  (declare-language-source-scan-worker
                                   bash-source-language)))))

(deflanguage-source-receipt parse-bash/receipt bash-source-language)
