;;; -*- Gerbil -*-
;;; Language declarations exercise one engine-owned deferred delimiter scanner.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role
                 make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-dispatch
                 compile-contextual-dispatch)
        (only-in :gerbil-parser/src/compiler/contextual-scanner-ir
                 compile-contextual-scanner)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 prepare-contextual-scanner prepare-contextual-scanner-plan contextual-scanner-initial-state
                 contextual-scanner-step contextual-scan-state-byte-offset
                 contextual-scan-state-mode contextual-scan-state-canonical
                 restore-contextual-scan-state)
        (only-in :gerbil-parser/src/runtime/token
                 token-kind token-lexeme))
(export contextual-deferred-scanner-test)

(def (method name mode position form result)
  (make-contextual-method name mode position form result))

(def (rule name mode form matcher rank action)
  (make-contextual-scan-rule name mode form matcher rank action))

(def bash-operators
  '(";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
    "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")"
    "<" ">"))

(def (deferred-scanner-ir owner marker-policy operators)
  (let* ((role
          (make-contextual-role
           owner
           (list (method 'operator 'any 'any 'operator 'operator)
                 (method 'here-open 'any 'any 'here-open 'here-open)
                 (method 'word 'any 'any 'word 'word)
                 (method 'space 'any 'any 'space 'horizontal-whitespace)
                 (method 'newline 'any 'any 'newline 'newline)
                 (method 'body 'any 'any 'body 'here-body)
                 (method 'end 'any 'any 'end 'here-end))))
         (dispatch
          (compile-contextual-dispatch
           (list role) '(command here-body) '(command)
           '(operator here-open word space newline body end)))
         (rules
          (list (rule 'here-strip 'command 'here-open
                      '(literal "<<-") 30 '(expect-marker #t))
                (rule 'here-plain 'command 'here-open
                      '(literal "<<") 30 '(expect-marker #f))
                (rule 'operator 'command 'operator
                      (list 'literals operators) 20 'keep)
                (rule 'word 'command 'word
                      (list 'balanced-word operators '("'" "\"") '())
                      0 (list 'enqueue-if-expecting marker-policy))
                (rule 'space 'command 'space
                      '(horizontal-whitespace+) 0 'keep)
                (rule 'newline 'command 'newline '(newline-one) 0
                      '(activate-next here-body))
                (rule 'end 'here-body 'end '(marker-line) 10
                      '(finish-marker command here-body))
                (rule 'body 'here-body 'body '(body-line) 0 'keep)))
         (ir (compile-contextual-scanner rules dispatch 'command)))
    ir))

(def (deferred-scanner source owner marker-policy operators)
  (prepare-contextual-scanner
   (prepare-contextual-scanner-plan (deferred-scanner-ir owner marker-policy operators)) source))

(def (scan-all scanner)
  (let loop ((state (contextual-scanner-initial-state scanner))
             (tokens '()))
    (let-values (((token next)
                  (contextual-scanner-step scanner state 'command)))
      (if token
        (loop next (cons token tokens))
        (values (reverse tokens) next)))))

(def contextual-deferred-scanner-test
  (test-suite "generic deferred delimiter scanner"
    (test-case "shared scanner plans keep delimiter queues source-local"
      (let* ((ir (deferred-scanner-ir 'plan-queues 'raw '("<<")))
             (plan (prepare-contextual-scanner-plan ir))
             (a (prepare-contextual-scanner plan "<<A\nα\nA\n"))
             (b (prepare-contextual-scanner plan "<<B\nβ\nB\n")))
        (def (queued scanner)
          (let-values (((open next) (contextual-scanner-step scanner (contextual-scanner-initial-state scanner) 'command)))
            (let-values (((word next) (contextual-scanner-step scanner next 'command))) next)))
        (check (cdr (assq 'pending (contextual-scan-state-canonical (queued a)))) => '(("A" #f #f)))
        (check (cdr (assq 'pending (contextual-scan-state-canonical (contextual-scanner-initial-state b)))) => '())
        (check (cdr (assq 'pending (contextual-scan-state-canonical (queued b)))) => '(("B" #f #f)))
        (let-values (((tokens final) (scan-all a)))
          (check (map token-lexeme tokens) => '("<<" "A" "\n" "α\n" "A\n")))
        (let-values (((tokens final) (scan-all b)))
          (check (map token-lexeme tokens) => '("<<" "B" "\n" "β\n" "B\n")))))
    (test-case "queued checkpoints preserve FIFO order across independent futures"
      (let* ((source "<<A <<B <<C\nα\nA\nβ\nB\nγ\nC\n")
             (scanner (deferred-scanner source 'queue-checkpoint 'raw '("<<")))
             (initial (contextual-scanner-initial-state scanner)))
        (def (advance state count)
          (if (zero? count) state
            (let-values (((_token next)
                          (contextual-scanner-step scanner state 'command)))
              (advance next (- count 1)))))
        (def (suffix state)
          (let loop ((state state) (trace '()))
            (let-values (((token next)
                          (contextual-scanner-step scanner state 'command)))
              (if token
                (loop next
                      (cons (list (token-kind token) (token-lexeme token)
                                  (contextual-scan-state-canonical next)) trace))
                (reverse trace)))))
        (let* ((after-a (advance initial 2))
               (receipt (contextual-scan-state-canonical after-a))
               (restored (restore-contextual-scan-state scanner receipt))
               (after-b (advance restored 3))
               (second (contextual-scan-state-canonical after-b)))
          (check (cdr (assq 'pending second)) => '(("A" #f #f) ("B" #f #f)))
          (check (suffix after-a) => (suffix restored))
          (check (suffix after-b)
                 => (suffix (restore-contextual-scan-state scanner second)))
          (check (map cadr (filter (lambda (row) (eq? (car row) 'here-end))
                                  (suffix restored)))
                 => '("A\n" "B\n" "C\n"))
          (check (contextual-scan-state-canonical after-a) => receipt))))
    (test-case "Bash here bodies preserve marker order and CRLF bytes"
      (let* ((source "cat <<'A' <<-B\r\n$x α\r\nA\r\n\tβ $x\r\n\tB\r\n")
             (scanner
              (deferred-scanner source 'bash-deferred
                                'shell-quote-removal bash-operators)))
        (let-values (((tokens final) (scan-all scanner)))
          (check (map token-kind
                      (filter (lambda (token)
                                (memq (token-kind token)
                                      '(here-body here-end)))
                              tokens))
                 => '(here-body here-end here-body here-end))
          (check (map token-lexeme
                      (filter (lambda (token)
                                (eq? (token-kind token) 'here-end))
                              tokens))
                 => '("A\r\n" "\tB\r\n"))
          (check (contextual-scan-state-mode final) => 'command)
          (check (contextual-scan-state-byte-offset final)
                 => (u8vector-length (string->utf8 source)))
          (check (cdr (assq 'pending (contextual-scan-state-canonical final)))
                 => '())
          (check (cdr (assq 'active (contextual-scan-state-canonical final)))
                 => #f))))
    (test-case "HCL raw marker uses the same deferred scanner"
      (let* ((source "<<EOF\nα\nEOF")
             (scanner (deferred-scanner source 'hcl-deferred
                                        'raw '("<<" "<<-"))))
        (let-values (((tokens final) (scan-all scanner)))
          (check (map token-kind tokens)
                 => '(here-open word newline here-body here-end))
          (check (token-lexeme (list-ref tokens 3)) => "α\n")
          (check (token-lexeme (list-ref tokens 4)) => "EOF")
          (check (contextual-scan-state-byte-offset final)
                 => (u8vector-length (string->utf8 source))))))
    (test-case "an unfinished marker fails at EOF"
      (let (scanner
            (deferred-scanner "cat <<EOF\nbody\n" 'bash-deferred
                              'shell-quote-removal bash-operators))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda () (scan-all scanner) #f))
         => "unfinished contextual scanner obligation")))
    (test-case "shell marker quote removal preserves ordinary backslashes"
      (let* ((source "cat <<\"\\q\"\npayload\n\\q\n")
             (scanner (deferred-scanner source 'bash-deferred
                                        'shell-quote-removal bash-operators)))
        (let-values (((tokens final) (scan-all scanner)))
          (check (apply string-append (map token-lexeme tokens)) => source)
          (check (map token-lexeme
                      (filter (lambda (token) (eq? (token-kind token) 'here-end))
                              tokens))
                 => '("\\q\n")))))
    (test-case "empty quoted shell marker closes on the blank line"
      (let* ((source "cat <<''\npayload\n\n")
             (scanner (deferred-scanner source 'bash-deferred
                                        'shell-quote-removal bash-operators)))
        (let-values (((tokens final) (scan-all scanner)))
          (check (apply string-append (map token-lexeme tokens)) => source)
          (check (map token-lexeme
                      (filter (lambda (token) (eq? (token-kind token) 'here-end))
                              tokens))
                 => '("\n")))))
    (test-case "checkpoint restores only for the same source and machine"
      (let* ((source "<<EOF\nα\nEOF\n")
             (scanner (deferred-scanner source 'hcl-deferred
                                        'raw '("<<" "<<-")))
             (initial (contextual-scanner-initial-state scanner)))
        (let-values (((_open after-open)
                      (contextual-scanner-step scanner initial 'command)))
          (let-values (((_marker after-marker)
                        (contextual-scanner-step scanner after-open 'command)))
            (let-values (((_newline active)
                          (contextual-scanner-step scanner after-marker
                                                   'command)))
              (let* ((receipt (contextual-scan-state-canonical active))
                     (restored
                      (restore-contextual-scan-state scanner receipt))
                     (other-source
                      (deferred-scanner "<<EOF\nβ\nEOF\n" 'hcl-deferred
                                        'raw '("<<" "<<-"))))
                (check (contextual-scan-state-canonical restored)
                       => receipt)
                (check
                 (with-catch
                  (lambda (condition) (error-message condition))
                  (lambda ()
                    (restore-contextual-scan-state other-source receipt)
                    #f))
                 => "contextual scanner checkpoint identity mismatch")
                (check
                 (with-catch
                  (lambda (condition) (error-message condition))
                  (lambda ()
                    (restore-contextual-scan-state
                     scanner
                     (map (lambda (row)
                            (if (eq? (car row) 'byteOffset)
                              (cons 'byteOffset 1) row))
                          receipt))
                    #f))
                 => "invalid contextual scanner checkpoint")))))))))
