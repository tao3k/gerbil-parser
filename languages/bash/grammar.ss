;;; -*- Gerbil -*-
;;; Bash syntax identity and declared nested word regions.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-source-support deflanguage-source ShellSourceStrategy. defscanner-profile ScannerProfile. defresult-profile ResultProfile. defpart-profile PartProfile. defbinding-profile BindingProfile. defcommand-profile CommandProfile.)
        (only-in :gerbil-parser/language-support defregion-plan defsyntax-corpus))
(export +bash-version+ +bash-syntax-contract+ bash-word-regions bash-command-scanner bash-results bash-commands bash-parts bash-simple-binding bash-parameter-binding bash-assignment-binding bash-fixtures bash-source-language)
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


(defbinding-profile (bash-simple-binding :: self BindingProfile.)
 (name (if-next (union (numeric) (characters "?@*#$!-_0"))
        (run (union (numeric) (characters "?@*#$!-_0")) 1 1)
        (run (union (alphabetic) (numeric) (characters "_")) 1 #f)))
 (prefixes) (operators) (subscript #f))
(defbinding-profile (bash-parameter-binding :: self BindingProfile.)
 (name (if-next (characters "@*?#$!-") (run (characters "@*?#$!-") 1 1)
        (if-next (numeric) (run (numeric) 1 #f)
         (run (union (alphabetic) (numeric) (characters "_")) 1 #f))))
 (prefixes "#" "!")
 (operators ":-" ":=" ":+" ":?" "##" "%%" "//" "^^" ",," "~~"
            ":" "-" "=" "+" "?" "#" "%" "/" "@" "^" "," "~")
 (subscript "[" #\]))
(defbinding-profile (bash-assignment-binding :: self BindingProfile.)
 (name (seq (run (union (alphabetic) (characters "_")) 1 1)
            (run (union (alphabetic) (numeric) (characters "_")) 0 #f)))
 (prefixes) (operators "=" "+=") (subscript #f))

(defpart-profile (bash-parts :: self PartProfile.)
 (contexts word SingleQuoted DoubleQuoted AnsiCString HereDocument)
 (bindings (simple bash-simple-binding) (parameter bash-parameter-binding) (assignment bash-assignment-binding))
 (rules
  ((word) (prefix "$'" any) (quote AnsiCString 2 1 #\'))
  ((word) (prefix "'" any) (quote SingleQuoted 1 1 #\'))
  ((word) (prefix "\"" any) (quote DoubleQuoted 1 1 #\"))
  ((word DoubleQuoted HereDocument) (prefix "${" any) (parameter))
  ((word DoubleQuoted HereDocument) (prefix "$((" any) (pair ArithmeticExpansion 3 2))
  ((word DoubleQuoted HereDocument) (prefix "$(" any) (pair CommandSubstitution 2 1))
  ((word) (prefix "<(" any) (pair ProcessSubstitution 2 1))
  ((word) (prefix ">(" any) (pair ProcessSubstitution 2 1))
  ((word DoubleQuoted HereDocument) (prefix "`" any) (quoted-body CommandSubstitution 1 1 #\`))
  ((word DoubleQuoted HereDocument) (prefix "$" next) (name SimpleParameter simple))
  ((word DoubleQuoted AnsiCString) (prefix "\\" any) (escape EscapeSequence))
  ((HereDocument) (prefix "\\" "$`\\\n") (escape EscapeSequence)))
 (literal LiteralPart))

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

(defcommand-profile (bash-commands :: self CommandProfile.)
 (roles
  (trivia (horizontal-whitespace) (comment) (line-continuation))
  (separator (newline) (operator ";" "&"))
  (redirect (operator "<" ">" ">>" "<>" "<<" "<<-" "<<<" "<&" ">&" ">|" "&>" "&>>"))
  (here-redirect (operator "<<" "<<-")) (strip-redirect (operator "<<-"))
  (reserved (word "if" "then" "elif" "else" "fi" "while" "until" "do" "done" "for" "select" "case" "in" "esac" "function" "{" "}"))
  (conditional-operator (word "==" "=" "!=" "=~" "-eq" "-ne" "-lt" "-le" "-gt" "-ge" "-z" "-n" "!"))
  (pipeline (operator "|" "|&")) (and-or (operator "&&" "||"))
  (case-end (operator ";;" ";&" ";;&")))
 (texts (descriptor (if-next (numeric) (run (numeric) 1 #f)
                 (seq (literal "{") (run (union (alphabetic) (characters "_")) 1 1)
                      (run (union (alphabetic) (numeric) (characters "_")) 0 #f) (literal "}")))))
 (forms
  (conditional-branch (word "if") 10 IfCommand
   (take keyword (word "if"))
   (list condition (word "then") #f) (take keyword (word "then"))
   (list body (word "elif" "else" "fi") #f)
   (many (word "elif") (take keyword (word "elif"))
     (list condition (word "then") #f) (take keyword (word "then"))
     (list body (word "elif" "else" "fi") #f))
   (optional (word "else") (take keyword (word "else")) (list else-body (word "fi") #f))
   (take keyword (word "fi")))
  (while-loop (word "while") 10 WhileCommand
   (take keyword (word "while")) (list condition (word "do") #f) (take keyword (word "do"))
   (list body (word "done") #f) (take keyword (word "done")))
  (until-loop (word "until") 10 UntilCommand
   (take keyword (word "until")) (list condition (word "do") #f) (take keyword (word "do"))
   (list body (word "done") #f) (take keyword (word "done")))
  (for-loop (word "for") 10 ForCommand
   (take keyword (word "for"))
   (branch (adjacent (operator "(") (operator "("))
     ((as ArithmeticForCommand) (call header arithmetic))
     ((word variable) (optional (word "in") (take keyword (word "in")) (many (word) (word item)))))
   (take separator (role separator)) (take keyword (word "do"))
   (list body (word "done") #f) (take keyword (word "done")))
  (select-loop (word "select") 10 SelectCommand
   (take keyword (word "select")) (word variable)
   (optional (word "in") (take keyword (word "in")) (many (word) (word item)))
   (take separator (role separator)) (take keyword (word "do"))
   (list body (word "done") #f) (take keyword (word "done")))
  (case-selection (word "case") 10 CaseCommand
   (take keyword (word "case")) (word subject) (take keyword (word "in"))
   (until (word "esac")
    (choose ((role separator) (raw separator))
     ((or (word) (operator "("))
      (node clause CaseClause
       (optional (operator "(") (take open (operator "(")))
       (word pattern) (many (operator "|") (take alternate (operator "|")) (word pattern))
       (take close (operator ")"))
       (list body (or (role case-end) (word "esac")) #t)
       (optional (role case-end) (raw terminator))))))
   (take keyword (word "esac")))
  (named-function (word "function") 10 FunctionDefinition
   (take keyword (word "function")) (word name)
   (optional (operator "(") (take open (operator "(")) (take close (operator ")"))) (command body))
  (function-header (lookahead (word) (operator "(") (operator ")")) 5 FunctionDefinition
   (word name) (take open (operator "(")) (take close (operator ")")) (command body))
  (conditional (word "[[") 0 ConditionalCommand
   (take open (word "[["))
   (until (word "]]") (choose ((role conditional-operator) (raw operator))
                            ((word) (word operand)) ((operator) (raw operator))))
   (take close (word "]]")))
  (arithmetic (adjacent (operator "(") (operator "(")) 4 ArithmeticCommand
   (take open (operator "(")) (take open (operator "("))
   (balance (operator "(") (operator ")") 2 open close
     (choose ((word) (word expression)) ((operator) (raw operator)))))
  (brace-group (word "{") 0 BraceGroup
   (take open (word "{")) (list body (word "}") #f) (take close (word "}")))
  (subshell (operator "(") 0 Subshell
   (take open (operator "(")) (list body (operator ")") #f) (take close (operator ")")))
  (array-tail (manual (operator "(")) 0 ArrayAssignment
   (take open (operator "("))
   (until (operator ")") (choose ((newline) (raw separator)) ((word) (word element))))
   (take close (operator ")")))
  (pipeline-prefix (manual) 0 Pipeline
   (optional (word "time") (take keyword (word "time")) (optional (word "-p") (take option (word "-p"))))
   (optional (word "!") (take negate (word "!")))))
)

(defsyntax-corpus bash-fixtures
  (identity "bash" "5.3" "bash-5.3-structured-source.v1")
  (accepted
   ("bash/heredoc" bash-heredoc (text "cat <<EOF\nα\nEOF\n") BashFile (HereDocument)))
  (rejected
   ("bash/incomplete-if" bash-incomplete-if (text "if true; then\n"))))

(deflanguage-source bash-source-language
  (identity "bash" +bash-version+ +bash-syntax-contract+)
  (strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions scanner: bash-command-scanner results: bash-results parts: bash-parts commands: bash-commands)))
