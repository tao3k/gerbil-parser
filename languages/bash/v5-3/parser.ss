;;; -*- Gerbil -*-
;;; Public versioned Bash parser entry and here-document span receipt.

(import (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser-loader LanguageLoader.
                 declare-language-source-scan-worker)
        (only-in :gerbil-parser/src/language/source
                 source-language-digest)
        (only-in ./grammar bash-v5-3-source-language)
        (only-in ./parser-core
                 bash-here-document-link?
                 bash-here-document-link-marker-start
                 bash-here-document-link-body-start
                 parse-bash-core/receipt)
        (only-in ./scanner bash-scan make-bash-scanner))
(export bash-v5-3-language
        parse-bash-v5-3
        parse-bash-v5-3/receipt
        bash-here-document-link?
        bash-here-document-link-marker-start
        bash-here-document-link-body-start)

(defsyntax-corpus bash-v5-3-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-parser-loader (bash-v5-3-language :: self LanguageLoader.)
  (source bash-v5-3-source-language)
  (parse parse-bash-v5-3)
  (slots metadata: (.o grammar-format: 'source-parser)
         fixtures: (lambda () bash-v5-3-fixtures)
         scan-workers: (list (cons 'command
                                  (declare-language-source-scan-worker
                                   bash-v5-3-source-language make-bash-scanner)))))

(def (parse-bash-v5-3/receipt source)
  (parse-bash-core/receipt
   source bash-scan (source-language-digest bash-v5-3-source-language)))
