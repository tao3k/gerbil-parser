;;; -*- Gerbil -*-
;;; Bash syntax cases are values; execution and invariant checks belong to the engine.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/language-support parse-artifact-success? parse-artifact-roundtrip)
        ./parser)
(deflanguage-parser-tests bash-parser-test "Bash source and structured syntax"
  (loader bash-language)
  (identity "public language identity" (language "bash") (version "5.3"))
  (accepted "nested parameter syntax"
    "printf %s pre\"${x:-$(printf '%s' '}')}\"post\n"
    (nodes ParameterExpansion CommandSubstitution) (tokens parameter-operator))
  (accepted "pipelines and logical operators"
    "time -p ! echo α | sed s/x/y/ && echo ok\n" (nodes Pipeline AndOrList))
  (accepted "assignment position and arithmetic expansion"
    "count=$((1 + (2 * 3))) echo \"$count\" a=b\n"
    (nodes Assignment ArithmeticExpansion) (counts (Assignment 1)))
  (accepted "array subscripts and parameter operators"
    "printf %s \"${array[@]:1:2}\" \"${#array[0]}\" \"${name^^}\"\n"
    (counts (ArraySubscript 2)) (tokens parameter-prefix parameter-operator))
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
  (fixtures "declared loader fixtures"))
