;;; -*- Gerbil -*-
;;; Bash syntax identity and declared nested word regions.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-source-support deflanguage-source ShellSourceStrategy. defscanner-profile ScannerProfile.)
        (only-in :gerbil-parser/language-support defregion-plan defsyntax-corpus))
(export +bash-version+ +bash-syntax-contract+ bash-word-regions bash-command-scanner bash-fixtures bash-source-language)
(def +bash-version+ "5.3")
(def +bash-syntax-contract+ "bash-5.3-structured-source.v1")

(defregion-plan bash-word-regions
  (stops ";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
         "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")" "<" ">")
  (quotes (#\' #f ()) (#\" #t ("${" "$(" "$((")) (#\` #t ()))
  (pairs ("${" #\{ #\} 1) ("$(" #\( #\) 1) ("$((" #\( #\) 2)
         ("<(" #\( #\) 1) (">(" #\( #\) 1))
  (consume-initial-stop #t))

(defscanner-profile (bash-command-scanner :: self ScannerProfile.)
 (modes command marker body) (initial-mode command)
 (tokens horizontal-whitespace newline line-continuation comment word operator
         heredoc-marker heredoc-content heredoc-end)
 (rules
  (space command horizontal-whitespace (horizontal-whitespace+) 0 keep)
  (newline command newline (literal "\n") 0 (activate-next body))
  (continuation command line-continuation (literal "\\\n") 10 keep)
  (comment command comment (line-prefix "#" "\n") 0 keep)
  (strip command operator (literal "<<-") 30 (expect-marker-in #t marker))
  (plain command operator (literal "<<") 30 (expect-marker-in #f marker))
  (operator command operator
   (literals (";;&" "&>>" "<<<" "&&" "||" "|&" ";;" ";&" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")" "<" ">")) 0 keep)
  (word command word (unless-prefix (" " "\t" "\n" "#" "\\\n" ";" "&" "|" "(" ")" "<" ">")
                    ("<(" ">(") (region-word bash-word-regions #t)) 0 keep)
  (marker-space marker horizontal-whitespace (horizontal-whitespace+) 0 keep)
  (marker-continuation marker line-continuation (literal "\\\n") 10 keep)
  (marker-newline marker newline (literal "\n") 10 (activate-next body))
  (marker-word marker heredoc-marker (unless-prefix (" " "\t" "\n" "\\\n") () (region-word bash-word-regions #t)) 0
               (enqueue-marker-in shell-quote-removal command))
  (end body heredoc-end (marker-line-at "\n") 10 (finish-marker command body))
  (body body heredoc-content (body-line-at "\n") 0 keep)))

(defsyntax-corpus bash-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-source bash-source-language
  (identity "bash" +bash-version+ +bash-syntax-contract+)
  (strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions scanner: bash-command-scanner)))
