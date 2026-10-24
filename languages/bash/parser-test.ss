;;; -*- Gerbil -*-
;;; Bash syntax cases are values; execution and invariant checks belong to the engine.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/language-support parse-artifact-success? parse-artifact-roundtrip)
        ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader. declare-language-source-scan-worker))
(export bash-fixtures bash-test-language)

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

(deflanguage-parser-tests bash-parser-test "Bash source and structured syntax"
  (loader bash-test-language)
  (identity "public language identity" (language "bash") (version "5.3"))
  (accepted "nested parameter syntax"
    "printf %s pre\"${x:-$(printf '%s' '}')}\"post\n"
    (nodes ParameterExpansion CommandSubstitution) (tokens parameter-operator))
  (accepted "pipelines and logical operators"
    "time -p ! echo α | sed s/x/y/ && echo ok\n" (nodes Pipeline AndOrList))
  (accepted "long pipelines use linear child publication"
    (string-append (apply string-append (make-list 2000 "echo α | ")) "echo ω\n")
    (nodes Pipeline) (counts (SimpleCommand 2001)))
  (accepted "large arrays use shared ordered child sequences"
    (string-append "values=(" (apply string-append (make-list 2000 "α ")) ")\n")
    (nodes ArrayAssignment) (counts (Word 2000)))
  (accepted "deferred FIFO preserves mixed here-document quote policies"
    "cat <<A <<'B' <<-C\n$x α\nA\n$y β\nB\n\t$z 中\n\tC\n"
    (counts (HereDocument 3) (HereDocumentLine 2)))
  (accepted "assignment position and arithmetic expansion"
    "count=$((1 + (2 * 3))) echo \"$count\" a=b\n"
    (nodes Assignment ArithmeticExpansion) (counts (Assignment 1)))
  (accepted "array subscripts and parameter operators"
    "printf %s \"${array[@]:1:2}\" \"${#array[0]}\" \"${name^^}\"\n"
    (counts (ArraySubscript 2)) (tokens parameter-prefix parameter-operator))
  (accepted-many "command substitutions retain literal brackets inside array indexes"
    '("echo ${a[$(echo [)]:-x}\n" "echo ${a[$(echo ] )]:-x}\n")
    (nodes ArraySubscript CommandSubstitution))
  (accepted "nested array indexes share scoped region captures"
    (string-append "echo " (apply string-append (make-list 512 "${x[")) "0"
                   (apply string-append (make-list 512 "]}")) "\n")
    (counts (ArraySubscript 512) (ParameterExpansion 512)))
  (accepted "array assignment has elements rather than a subshell"
    "values=(one two [key]=three)\n" (nodes ArrayAssignment) (without-nodes Subshell))
  (accepted-many "delimiter quote removal retains literal backslashes"
    '("cat <<\"a\\q\"\n$x α\na\\q\n" "cat <<A\\\nB\nα\nAB\n"))
  (accepted "here-document line structure"
    "cat <<'A' <<-B\n$x α\nA\n\tβ $x\n\tB\n" (counts (HereDocumentLine 1)))
  (property "here-document receipt links retain lexical order"
    (bindings (source "cat <<'A' <<-B\n$x α\nA\n\tβ $x\n\tB\n")
              (result (call-with-values (lambda () (parse-bash/receipt source)) list))
              (artifact (car result)) (links (cadr result)))
    (equal (parse-artifact-success? artifact) #t)
    (equal (parse-artifact-roundtrip artifact) source)
    (equal (length links) 2)
    (equal (< (shell-here-document-link-marker-start (car links))
              (shell-here-document-link-marker-start (cadr links))) #t)
    (equal (< (shell-here-document-link-body-start (car links))
              (shell-here-document-link-body-start (cadr links))) #t))
  (accepted "compound command lists"
    (string-append "if true; then { echo yes; }; else (echo no); fi\n"
                   "while test x; do echo x; done\n")
    (nodes IfCommand BraceGroup Subshell WhileCommand CommandList))
  (accepted "long conditional chains use the same consuming program"
    (string-append "if true; then echo α; "
                   (apply string-append (make-list 256 "elif true; then echo 中; "))
                   "else echo ω; fi\n")
    (counts (IfCommand 1)))
  (accepted "case clauses share declared node programs"
    (string-append "case x in " (apply string-append (make-list 2000 "a) echo α;; ")) "esac\n")
    (counts (CaseClause 2000)))
  (accepted "nested groups use declared command calls"
    (string-append (apply string-append (make-list 256 "{ ")) "echo α; "
                   (apply string-append (make-list 256 "}; ")) "\n")
    (counts (BraceGroup 256)))
  (accepted "for, case and function bodies"
    (string-append "for item in a b; do echo \"$item\"; done\n"
                   "case $item in a|b) echo ok;; *) echo no;;& esac\n" "f() { echo done; }\n")
    (nodes ForCommand CaseCommand CaseClause FunctionDefinition))
  (accepted "arithmetic for and select headers"
    (string-append "for ((i=0; i<3; i++)); do echo $i; done\n"
                   "select item in a b; do echo $item; done\n")
    (nodes ArithmeticForCommand SelectCommand))
  (accepted "conditionals and compound redirects"
    "[[ -n \"$x\" && $x == yes ]] && ((count += 1))\n{ echo hi; } >out\n"
    (nodes ConditionalCommand ArithmeticCommand RedirectedCommand))
  (accepted "file descriptors and read-write redirects"
    "{fd}<>data echo ok 2>>errors\n" (counts (Redirection 2)))
  (rejected "unterminated compound has diagnostics" "case x in a) echo a\n" (diagnostics))
  (rejected-many "empty required lists and missing targets"
    '("if true; then fi\n" "while true; do done\n" "{ ; }\n" "echo >\n" "cat <<EOF\nbody\n"))
  (accepted "many Unicode word parts retain lossless token order"
    (string-append "printf %s " (apply string-append (make-list 2000 "α$x")) "\n")
    (counts (SimpleParameter 2000)))
  (accepted "nested parameter operands retain all structured results"
    (string-append "echo " (apply string-append (make-list 2000 "${x:-"))
                   "中😀" (make-string 2000 #\}) "\n")
    (counts (ParameterExpansion 2000)))
  (native-entry "source descriptor uses derived result catalog" "echo \"α${x:-中}\"\n" accepted)
  (native-entry "source rejection uses admitted fallback terminal" "if true; then\n" rejected)
  (fixtures "declared loader fixtures"))
