;;; -*- Gerbil -*-
;;; Bash syntax identity and declared nested word regions.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-source-support deflanguage-source ShellSourceStrategy.)
        (only-in :gerbil-parser/language-support defregion-plan defsyntax-corpus))
(export +bash-version+ +bash-syntax-contract+ bash-word-regions bash-fixtures bash-source-language)
(def +bash-version+ "5.3")
(def +bash-syntax-contract+ "bash-5.3-structured-source.v1")

(defregion-plan bash-word-regions
  (stops ";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
         "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")" "<" ">")
  (quotes (#\' #f ()) (#\" #t ("${" "$(" "$((")) (#\` #t ()))
  (pairs ("${" #\{ #\} 1) ("$(" #\( #\) 1) ("$((" #\( #\) 2)
         ("<(" #\( #\) 1) (">(" #\( #\) 1))
  (consume-initial-stop #t))

(defsyntax-corpus bash-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-source bash-source-language
  (identity "bash" +bash-version+ +bash-syntax-contract+)
  (strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions)))
