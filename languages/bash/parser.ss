;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader LanguageLoader.)
        (only-in :gerbil-parser/language-source-support deflanguage-parser-receipt)
        (only-in :gerbil-parser/src/runtime/shell-parser
                 shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)
        ./grammar)
(export (import: ./grammar) bash-language parse-bash parse-bash/receipt
        shell-here-document-link? shell-here-document-link-marker-start shell-here-document-link-body-start)

(deflanguage-parser-loader (bash-language :: self LanguageLoader.)
  (source bash-source-language)
  (parse parse-bash)
  (slots metadata: (.o grammar-format: 'source-parser)))

(deflanguage-parser-receipt parse-bash/receipt bash-source-language)
