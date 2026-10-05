;;; -*- Gerbil -*-
;;; Bash source and syntax receipts: byte-exact round trips, structured words,
;;; and ordered here-document bodies.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/language/entry
                 language-parser-entry-ref)
        (only-in :gerbil-parser/src/language/source
                 declare-source-language parse-source-language)
        (only-in :gerbil-parser/languages/bash/parser
                 bash-language
                 bash-here-document-link-marker-start
                 bash-here-document-link-body-start
                 parse-bash parse-bash/receipt)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-events parse-artifact-ref
                 parse-artifact-roundtrip parse-artifact-success?
                 token-event? token-event-token-kind)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state
                 source-scanner-step source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token
                 token-end token-kind token-start))

(def (token-kinds artifact)
  (map token-event-token-kind
       (filter token-event? (parse-artifact-events artifact))))

(def (node-kinds artifact)
  (map (lambda (event) (vector-ref event 2))
       (filter (lambda (event) (eq? (vector-ref event 0) 'start-node))
               (parse-artifact-events artifact))))

(def (accepted-roundtrip? source)
  (let (artifact (parse-bash source))
    (and (parse-artifact-success? artifact)
         (string=? (parse-artifact-roundtrip artifact) source))))

(def bash-tests
  (test-suite "Bash 5.3 source and structured syntax"
    (test-case "public entry retains its declared language identity"
      (check (language-parser-entry-ref bash-language 'language)
             => "bash")
      (check (language-parser-entry-ref bash-language 'version)
             => "5.3"))
    (test-case "source language rejects an artifact with another digest"
      (let (other
            (declare-source-language
             "bash" "5.3" "different-contract"
             (lambda (_source) '())
             (lambda (source _scanner _digest)
               (parse-bash source))))
        (check
         (with-catch
          (lambda (_condition) #t)
          (lambda ()
            (parse-source-language other "echo hi\n")
            #f))
         => #t)))
    (test-case "scanner checkpoints retain byte offsets"
      (let* ((scanner
              (make-source-scanner
               "αx" #f
               (lambda (_source offset context _mode)
                 (if (= offset 2)
                   (values #f offset context)
                   (values 'character (fx+ offset 1) context)))))
             (initial (source-scanner-initial-state scanner)))
        (let-values (((first after-first)
                      (source-scanner-step scanner initial 'test)))
          (check (token-start first) => 0)
          (check (token-end first) => 2)
          (check (source-scan-state-byte-offset initial) => 0)
          (check (source-scan-state-byte-offset after-first) => 2)
          (let-values (((second after-second)
                        (source-scanner-step scanner after-first 'test)))
            (check (token-kind second) => 'character)
            (check (token-start second) => 2)
            (check (token-end second) => 3)
            (check (source-scan-state-byte-offset after-second) => 3)))))
    (test-case "word parts preserve nested parameter syntax"
      (let* ((source "printf %s pre\"${x:-$(printf '%s' '}')}\"post\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (and (memq 'ParameterExpansion (node-kinds artifact)) #t)
               => #t)
        (check (and (memq 'CommandSubstitution (node-kinds artifact)) #t)
               => #t)
        (check (and (memq 'parameter-operator (token-kinds artifact)) #t)
               => #t)))
    (test-case "pipelines and logical operators retain syntax"
      (let* ((source "time -p ! echo α | sed s/x/y/ && echo ok\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (and (memq 'Pipeline (node-kinds artifact)) #t) => #t)
        (check (and (memq 'AndOrList (node-kinds artifact)) #t) => #t)))
    (test-case "assignment position and arithmetic expansion are explicit"
      (let* ((source "count=$((1 + (2 * 3))) echo \"$count\" a=b\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (and (memq 'Assignment (node-kinds artifact)) #t) => #t)
        (check (and (memq 'ArithmeticExpansion (node-kinds artifact)) #t)
               => #t)
        (check (length
                (filter (lambda (kind) (eq? kind 'Assignment))
                        (node-kinds artifact)))
               => 1)))
    (test-case "array subscripts and parameter operators keep their spans"
      (let* ((source
              "printf %s \"${array[@]:1:2}\" \"${#array[0]}\" \"${name^^}\"\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (length
                (filter (lambda (kind) (eq? kind 'ArraySubscript))
                        (node-kinds artifact)))
               => 2)
        (check (and (memq 'parameter-prefix (token-kinds artifact)) #t)
               => #t)
        (check (and (memq 'parameter-operator (token-kinds artifact)) #t)
               => #t)))
    (test-case "array assignments have elements rather than a subshell"
      (let* ((source "values=(one two [key]=three)\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (and (memq 'ArrayAssignment (node-kinds artifact)) #t)
               => #t)
        (check (and (memq 'Subshell (node-kinds artifact)) #t)
               => #f)))
    (test-case "here-documents bind to markers in lexical order"
      (let* ((source "cat <<'A' <<-B\n$x α\nA\n\tβ $x\n\tB\n")
             (artifact #f)
             (links #f))
        (let-values (((parsed found) (parse-bash/receipt source)))
          (set! artifact parsed)
          (set! links found))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (length links) => 2)
        (check (length
                (filter (lambda (kind) (eq? kind 'HereDocumentLine))
                        (node-kinds artifact)))
               => 1)
        (check (< (bash-here-document-link-marker-start (car links))
                  (bash-here-document-link-marker-start (cadr links))) => #t)
        (check (< (bash-here-document-link-body-start (car links))
                  (bash-here-document-link-body-start (cadr links))) => #t)))
    (test-case "compound command lists are structured"
      (let* ((source
              (string-append
               "if true; then { echo yes; }; else (echo no); fi\n"
               "while test x; do echo x; done\n"))
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (for-each
         (lambda (kind)
           (check (and (memq kind (node-kinds artifact)) #t) => #t))
         '(IfCommand BraceGroup Subshell WhileCommand CommandList))))
    (test-case "for, case, and function bodies have distinct nodes"
      (let* ((source
              (string-append
               "for item in a b; do echo \"$item\"; done\n"
               "case $item in a|b) echo ok;; *) echo no;;& esac\n"
               "f() { echo done; }\n"))
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (for-each
         (lambda (kind)
           (check (and (memq kind (node-kinds artifact)) #t) => #t))
         '(ForCommand CaseCommand CaseClause FunctionDefinition))))
    (test-case "arithmetic for and select headers retain source order"
      (let* ((source
              (string-append
               "for ((i=0; i<3; i++)); do echo $i; done\n"
               "select item in a b; do echo $item; done\n"))
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (and (memq 'ArithmeticForCommand (node-kinds artifact)) #t)
               => #t)
        (check (and (memq 'SelectCommand (node-kinds artifact)) #t)
               => #t)))
    (test-case "conditionals and compound redirects retain their shape"
      (let* ((source
              "[[ -n \"$x\" && $x == yes ]] && ((count += 1))\n{ echo hi; } >out\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (for-each
         (lambda (kind)
           (check (and (memq kind (node-kinds artifact)) #t) => #t))
         '(ConditionalCommand ArithmeticCommand RedirectedCommand))))
    (test-case "file-descriptor and read-write redirects stay attached"
      (let* ((source "{fd}<>data echo ok 2>>errors\n")
             (artifact (parse-bash source)))
        (check (accepted-roundtrip? source) => #t)
        (check (length
                (filter (lambda (kind) (eq? kind 'Redirection))
                        (node-kinds artifact)))
               => 2)))
    (test-case "unterminated compound syntax rejects with a diagnostic"
      (let (artifact (parse-bash "case x in a) echo a\n"))
        (check (parse-artifact-success? artifact) => #f)
        (check (pair? (parse-artifact-ref artifact 'diagnostics)) => #t)))
    (test-case "empty required lists and missing targets reject"
      (for-each
       (lambda (source)
         (check (parse-artifact-success? (parse-bash source))
                => #f))
       '("if true; then fi\n"
         "while true; do done\n"
         "{ ; }\n"
         "echo >\n"
         "cat <<EOF\nbody\n")))))

(def bash-test bash-tests)
(export bash-test)
