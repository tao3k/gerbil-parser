;;; Shared IR fixture: Bash FIFO delimiters and contextual positions.
(import (only-in :gerbil-parser/t/fixtures/bash-products bash-word-regions)
        (only-in :gerbil-parser/src/runtime/region-scanner region-plan-specification)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-dispatch compile-contextual-dispatch)
        (only-in :gerbil-parser/src/compiler/contextual-scanner-ir compile-contextual-scanner))
(export shared-scanner-ir shared-scanner-sources)
(def shared-scanner-sources
  '("cat <<'A' <<-B\r\n$x α\r\nA\r\n\tβ $x\n\tB\n"
    "cat <<\\\\EOF\ntext\n\\EOF\n"
    "cat <<\"a\\qb\"\na\\qb\n"
    "cat <<''\n\n"
    "printf %s \"${x:-$(printf '%s' '}')}\"\n"
    "echo α\n\n"
    "echo $((1+(2*3))) <(printf 'α') >(cat)\n"
    "echo `opaque ${x}`\n"
    "echo α$(printf 'β')$((2))\n"))
(def role
  (make-contextual-role 'shared
    (map (lambda (row) (make-contextual-method (car row) 'any 'any (car row) (cadr row)))
         '((open here-open) (word word) (space space) (newline newline) (body body) (end end)))))
(def dispatch (compile-contextual-dispatch (list role) '(command body) '(command argument) '(open word space newline body end)))
(def (rule name mode form matcher rank (action 'keep))
  (make-contextual-scan-rule name mode form matcher rank action))
(def shared-scanner-ir
  (compile-contextual-scanner
   (list (rule 'strip 'command 'open '(literal "<<-") 30 '(expect-marker #t))
         (rule 'plain 'command 'open '(literal "<<") 30 '(expect-marker #f))
         (rule 'word 'command 'word
               (list 'region-word
                     (append (take (region-plan-specification bash-word-regions) 3) '(#f)))
               0 '(enqueue-if-expecting shell-quote-removal))
         (rule 'space 'command 'space '(horizontal-whitespace+) 0)
         (rule 'newline 'command 'newline '(newline-one) 0 '(activate-next body))
         (rule 'marker 'body 'end '(marker-line) 10 '(finish-marker command body))
         (rule 'body 'body 'body '(body-line) 0)) dispatch 'command))
