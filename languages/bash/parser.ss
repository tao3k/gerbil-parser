;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.
                 declare-language-source-scan-worker)
        (only-in :gerbil-parser/src/language/source
                 source-language-digest declare-source-language)
        ./grammar
        (only-in ./parser-core
                 bash-here-document-link?
                 bash-here-document-link-marker-start
                 bash-here-document-link-body-start
                 parse-bash-core parse-bash-core/receipt)
        (only-in ./scanner bash-scan make-bash-scanner))
(export (import: ./grammar) bash-source-language bash-language
        parse-bash
        parse-bash/receipt
        bash-here-document-link?
        bash-here-document-link-marker-start
        bash-here-document-link-body-start)

(def bash-source-language
  (declare-source-language "bash" +bash-version+ +bash-syntax-contract+
                          bash-scan parse-bash-core))

(deflanguage-parser-loader (bash-language :: self LanguageLoader.)
  (source bash-source-language)
  (parse parse-bash)
  (slots metadata: (.o grammar-format: 'source-parser)
         fixtures: bash-fixtures
         scan-workers: (list (cons 'command
                                  (declare-language-source-scan-worker
                                   bash-source-language make-bash-scanner)))))

(def (parse-bash/receipt source)
  (parse-bash-core/receipt
   source bash-scan (source-language-digest bash-source-language)))
