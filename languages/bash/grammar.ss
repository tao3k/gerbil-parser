;;; -*- Gerbil -*-
;;; Bash syntax identity and declared nested word regions.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-source-support deflanguage-source ShellSourceStrategy. defscanner-profile ScannerProfile. defresult-profile ResultProfile.)
        (only-in :gerbil-parser/language-support defregion-plan defsyntax-corpus))
(export +bash-version+ +bash-syntax-contract+ bash-word-regions bash-command-scanner bash-results bash-fixtures bash-source-language)
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


(defresult-profile (bash-results :: self ResultProfile.)
 (scanner bash-command-scanner)
 (nodes
  (Word part) (HereDocumentLine part) (Assignment name operator value)
  (LiteralPart text) (EscapeSequence text) (SimpleParameter text)
  (SingleQuoted open part close) (DoubleQuoted open part close) (AnsiCString open part close)
  (ParameterExpansion open prefix name subscript operator operand close)
  (ArraySubscript open index close)
  (ArithmeticExpansion open body close) (CommandSubstitution open body close)
  (ProcessSubstitution open body close)
  (Redirection descriptor operator target) (ArrayAssignment assignment open close separator element)
  (SimpleCommand assignment name argument redirect) (CommandList command separator here-document)
  (IfCommand keyword condition body else-body) (WhileCommand keyword condition body)
  (UntilCommand keyword condition body) (ForCommand keyword header variable item separator body)
  (SelectCommand keyword header variable item separator body)
  (ArithmeticForCommand keyword header variable item separator body)
  (CaseCommand keyword subject separator clause) (CaseClause open pattern alternate close body terminator)
  (FunctionDefinition keyword name open close body) (ConditionalCommand open close operator operand)
  (ArithmeticCommand open close expression operator) (RedirectedCommand command redirect)
  (BraceGroup open body close) (Subshell open body close)
  (Pipeline keyword option negate command operator) (AndOrList command operator)
  (HereDocument delimiter content) (BashFile command separator here-document))
 (tokens unparsed-source
         LiteralPart EscapeSequence SimpleParameter
         parameter-open parameter-prefix parameter-name parameter-operator parameter-close
         subscript-open subscript-close quote-open quote-close
         substitution-open substitution-body substitution-close assignment-name assignment-operator))

(defsyntax-corpus bash-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-source bash-source-language
  (identity "bash" +bash-version+ +bash-syntax-contract+)
  (strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions scanner: bash-command-scanner results: bash-results)))
