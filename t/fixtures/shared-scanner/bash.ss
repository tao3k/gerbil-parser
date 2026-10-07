;;; Production Bash scanner profile and portable conformance source controls.
(import (only-in :gerbil-parser/t/fixtures/bash-products bash-command-scanner)
        (only-in :gerbil-parser/src/language/scanner-profile compile-scanner-profile))
(export bash-scanner-ir bash-scanner-sources)
(def bash-scanner-ir (compile-scanner-profile bash-command-scanner))
(def bash-scanner-sources
(append
     '(
      "" "echo α" "# unterminated '\n" "# $(unterminated\n"
      "echo \\\nα" "echo <(printf α) >(cat)" "echo \rα" "echo \r\n" "\rfoo" "\x85;α"
      "cat <<A <<B\nα\nA\nβ\nB\n" "cat <<-A\n\tα\n\tA\n"
      "cat <<'A'\n$α\nA\n" "cat <<\"a\\q\"\nα\na\\q\n"
      "cat <<''\nα\n\n" "cat <<A\\\nB\nα\nAB\n"
      "cat <<A \\\n <<B\nα\nA\nβ\nB\n"
      "cat <<A\r\nα\r\nA\r\n" "cat <<\n" "cat <<A" "cat <<A\n"
      "cat <<A\nα\nA" "cat <<#x\nα\n#x\n" "cat <<;\nα\n;\n"
      "cat <<$(x)\nα\n$(x)\n" "echo '${x'" "echo \"${α:-$(x)}\""
      "echo <<<A" "echo <<-A\nA\n")
     (map (lambda (operator) (string-append "x" operator "y"))
      '(";;&" "&>>" "<<<" "&&" "||" "|&" ";;" ";&" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")" "<" ">"))))
