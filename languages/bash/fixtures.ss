;;; Shared corpus and development loader declarations; no Suite execution.
(import ./parser
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader. declare-language-source-scan-worker))
(export #t)

(defsyntax-corpus bash-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-development-loader bash-test-language
  (source bash-source-language)
  (parse parse-bash-test)
  (slots metadata: '((grammar-format . source-parser))
         fixtures: bash-fixtures
         scan-workers: (list (cons 'command
                                  (declare-language-source-scan-worker
                                   bash-source-language)))))
