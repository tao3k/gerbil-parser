;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :gerbil-parser/src/language/entry deflanguage-parser)
        (only-in :gerbil-parser/src/language/source
                 source-language-digest)
        (only-in ./grammar bash-v5-3-source-language)
        (only-in ./parser-core
                 bash-here-document-link?
                 bash-here-document-link-marker-start
                 bash-here-document-link-body-start
                 parse-bash-core/receipt)
        (only-in ./scanner bash-scan))
(export bash-v5-3-language
        parse-bash-v5-3
        parse-bash-v5-3/receipt
        bash-here-document-link?
        bash-here-document-link-marker-start
        bash-here-document-link-body-start)

(deflanguage-parser bash-v5-3-language
  (source bash-v5-3-source-language)
  (parse parse-bash-v5-3))

(def (parse-bash-v5-3/receipt source)
  (parse-bash-core/receipt
   source bash-scan (source-language-digest bash-v5-3-source-language)))
