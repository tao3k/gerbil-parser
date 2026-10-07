;;; -*- Gerbil -*-
;;; Bash author grammar. The source library derives and admits execution plans.
(import (only-in :gerbil-parser/language-support/grammar deflanguage defgrammar-syntax)
        (only-in :gerbil-parser/language-support/shell shell))
(export bash-syntax)

(defgrammar-syntax (compound-loop kind opener)
  (node kind (field keyword opener) (field condition command-list)
    (field keyword "do") (field body command-list) (field keyword "done")))

(deflanguage bash-syntax
 (syntax
  (shell
   (operators ";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
     "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")" "<" ">")
   (regions
    (quote single SingleQuoted "'" opaque)
    (quote double DoubleQuoted "\"" escaped "${" "$(" "$((")
    (quote backtick CommandSubstitution "`" escaped)
    (pair parameter ParameterExpansion "${" "}")
    (pair command CommandSubstitution "$(" ")")
    (pair arithmetic ArithmeticExpansion "$((" "))")
    (pair input ProcessSubstitution "<(" ")")
    (pair output ProcessSubstitution ">(" ")")
    (quote ansi AnsiCString "$'" opaque))
   (bindings
    (simple (if-next (union (numeric) (characters "?@*#$!-_0"))
             (run (union (numeric) (characters "?@*#$!-_0")) 1 1)
             (run (union (alphabetic) (numeric) (characters "_")) 1 #f)) () () #f)
    (parameter (if-next (characters "@*?#$!-") (run (characters "@*?#$!-") 1 1)
                (if-next (numeric) (run (numeric) 1 #f)
                  (run (union (alphabetic) (numeric) (characters "_")) 1 #f)))
     ("#" "!") (":-" ":=" ":+" ":?" "##" "%%" "//" "^^" ",," "~~"
                ":" "-" "=" "+" "?" "#" "%" "/" "@" "^" "," "~") ("[" #\]))
    (assignment (seq (run (union (alphabetic) (characters "_")) 1 1)
                    (run (union (alphabetic) (numeric) (characters "_")) 0 #f)) () ("=" "+=") #f))
   (words
    ((word) (quoted ansi)) ((word) (quoted single)) ((word) (quoted double))
    ((word DoubleQuoted HereDocument) (parameter parameter))
    ((word DoubleQuoted HereDocument) (substitution arithmetic))
    ((word DoubleQuoted HereDocument) (substitution command))
    ((word) (substitution input)) ((word) (substitution output))
    ((word DoubleQuoted HereDocument) (substitution backtick))
    ((word DoubleQuoted HereDocument) (name SimpleParameter "$" simple))
    ((word DoubleQuoted AnsiCString) (escape EscapeSequence any))
    ((HereDocument) (escape EscapeSequence "$`\\\n")))
   (commands
    (roles
     (trivia (horizontal-whitespace) (comment) (line-continuation))
     (separator (newline) (operator ";" "&"))
     (redirect (operator "<" ">" ">>" "<>" "<<" "<<-" "<<<" "<&" ">&" ">|" "&>" "&>>"))
     (here-redirect (operator "<<" "<<-")) (strip-redirect (operator "<<-"))
     (reserved (word "if" "then" "elif" "else" "fi" "while" "until" "do" "done" "for" "select" "case" "in" "esac" "function" "{" "}"))
     (conditional-operator (word "==" "=" "!=" "=~" "-eq" "-ne" "-lt" "-le" "-gt" "-ge" "-z" "-n" "!"))
     (pipeline (operator "|" "|&")) (and-or (operator "&&" "||")) (case-end (operator ";;" ";&" ";;&")))
    (descriptor (if-next (numeric) (run (numeric) 1 #f)
                  (seq (literal "{") (run (union (alphabetic) (characters "_")) 1 1)
                       (run (union (alphabetic) (numeric) (characters "_")) 0 #f) (literal "}"))))
   )))
 (rules
     (conditional-branch
      (node IfCommand (field keyword "if") (field condition command-list) (field keyword "then")
       (field body command-list)
       (repeat (seq (field keyword "elif") (field condition command-list) (field keyword "then") (field body command-list)))
       (optional (seq (field keyword "else") (field else-body command-list))) (field keyword "fi")))
     (while-loop (compound-loop WhileCommand "while"))
     (until-loop (compound-loop UntilCommand "until"))
     (for-loop
      (node ForCommand (field keyword "for")
       (choice (node ArithmeticForCommand (field header (reference arithmetic)))
               (seq (field variable word) (optional (seq (field keyword "in") (repeat (field item word))))))
       (field separator (role separator)) (field keyword "do") (field body command-list) (field keyword "done")))
     (select-loop
      (node SelectCommand (field keyword "select") (field variable word)
       (optional (seq (field keyword "in") (repeat (field item word))))
       (field separator (role separator)) (field keyword "do") (field body command-list) (field keyword "done")))
     (case-selection
      (node CaseCommand (field keyword "case") (field subject word) (field keyword "in")
       (repeat (choice (field separator (role separator))
        (field clause (node CaseClause (optional (field open (operator "(")))
          (field pattern word) (repeat (seq (field alternate (operator "|")) (field pattern word)))
          (field close (operator ")")) (field body (empty-allowed command-list))
          (optional (field terminator (role case-end)))))))
       (field keyword "esac")))
     (named-function
      (node FunctionDefinition (field keyword "function") (field name word)
       (optional (seq (field open (operator "(")) (field close (operator ")")))) (field body command)))
     (function-header
      (node FunctionDefinition (field name word) (field open (operator "(")) (field close (operator ")")) (field body command)))
     (conditional
      (node ConditionalCommand (field open "[[")
       (repeat (choice (field operator (role conditional-operator)) (field operand word) (field operator (operator))))
       (field close "]]")))
     (arithmetic
      (node ArithmeticCommand
       (balanced (field open (adjacent (operator "(") (operator "(")))
                 (field close (adjacent (operator ")") (operator ")")))
                 (choice (field expression word) (field operator (operator))))))
     (brace-group (node BraceGroup (field open "{") (field body command-list) (field close "}")))
     (subshell (node Subshell (field open (operator "(")) (field body command-list) (field close (operator ")"))))
     (array-tail
      (node ArrayAssignment (field open (operator "("))
       (repeat (choice (field separator (newline)) (field element word))) (field close (operator ")"))))
     (pipeline-prefix
      (node Pipeline (optional (seq (field keyword "time") (optional (field option "-p"))))
                     (optional (field negate "!"))))))
