;;; -*- Gerbil -*-
;;; Language declarations exercise one engine-owned deferred delimiter scanner.

(import (only-in :std/test check check-exception test-case test-suite)
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
                 restore-contextual-scan-state contextual-scan-state-converged?
                 contextual-scanner-replay-prefix contextual-scan-state-with-mode
                 prepare-contextual-scan-suffix contextual-scan-suffix-state)
        (only-in :gerbil-parser/src/runtime/token
                 token-kind token-lexeme token-start token-end))
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
    (test-case "prefix replay reaches source-owned HCL and Bash delimiter states"
      (for-each
       (lambda (policy)
         (let* ((plan (prepare-contextual-scanner-plan
                       (deferred-scanner-ir 'prefix-replay policy '("<<" "<<-" "<<<"))))
                (old (prepare-contextual-scanner plan "猫 <<A\nα\nA\n"))
                (new (prepare-contextual-scanner plan "猫 <<A\nβ\nA\n")))
           (def (advance scanner count)
             (let loop ((state (contextual-scanner-initial-state scanner)) (n count))
               (if (zero? n) state
                 (let-values (((token next) (contextual-scanner-step scanner state 'command)))
                   (loop next (- n 1))))))
           (for-each
            (lambda (count)
              (let* ((checkpoint (advance old count))
                     (receipt (contextual-scan-state-canonical checkpoint))
                     (replayed (contextual-scanner-replay-prefix new checkpoint (make-list count 'command))))
                (check (contextual-scan-state-canonical replayed)
                       => (contextual-scan-state-canonical (advance new count)))
                (check (contextual-scan-state-canonical checkpoint) => receipt)
                (check-exception (contextual-scanner-step new checkpoint 'command) true)
                (let-values (((token next) (contextual-scanner-step new replayed 'command)))
                  (check (and token #t) => #t))))
            '(0 4 5))
           (let (checkpoint (advance old 5))
             (check-exception (contextual-scanner-replay-prefix new checkpoint '(command)) true)
             (check-exception (contextual-scanner-replay-prefix new checkpoint (make-list 6 'command)) true)
             (check-exception (contextual-scanner-replay-prefix new checkpoint '(unknown)) true)
             (check-exception
              (contextual-scanner-replay-prefix new (contextual-scan-state-with-mode checkpoint 'command)
                                               (make-list 5 'command)) true))
           (for-each
            (lambda (sources)
              (let* ((a (prepare-contextual-scanner plan (car sources)))
                     (b (prepare-contextual-scanner plan (cadr sources)))
                     (checkpoint (advance a 1)))
                ;; The prefix bytes match, but actual longest-match/EOF/quote decisions change.
                (check-exception (contextual-scanner-replay-prefix b checkpoint '(command)) true)))
            '(("cat" "catx") ("<<A" "<<-A") ("\"x\"" "\"x\"tail") ("猫 " "犬 ") ("cat " "ca")))
           (let ((foreign (prepare-contextual-scanner
                           (prepare-contextual-scanner-plan
                            (deferred-scanner-ir 'prefix-replay policy '("<<" "<<-" "<<<")))
                           "猫 <<A\nβ\nA\n")))
             (check-exception (contextual-scanner-replay-prefix foreign (advance old 5)
                                                              (make-list 5 'command)) true))))
       '(raw shell-quote-removal)))
    (test-case "edited HCL and Bash prefixes converge only with the complete lexical context"
      (for-each
       (lambda (policy)
         (let* ((plan (prepare-contextual-scanner-plan
                       (deferred-scanner-ir 'convergence policy '("<<" "<<-"))))
                (a (prepare-contextual-scanner plan "cat <<A\nα\nA\n"))
                (b (prepare-contextual-scanner plan "猫猫 <<A\nα\nA\n")))
           (def (advance scanner count)
             (let loop ((state (contextual-scanner-initial-state scanner)) (n count))
               (if (zero? n) state
                 (let-values (((token next) (contextual-scanner-step scanner state 'command)))
                   (loop next (- n 1))))))
           (def (trace scanner state origin)
             (let-values (((token next) (contextual-scanner-step scanner state 'command)))
               (if token
                 (cons (list (token-kind token) (token-lexeme token)
                             (- (token-start token) origin) (- (token-end token) origin))
                       (trace scanner next origin)) '())))
           (let* ((old (advance a 5)) (new (advance b 5))
                  (receipt (contextual-scan-state-canonical new)))
             (check (contextual-scan-state-converged? old new) => #t)
             (check (trace a old (contextual-scan-state-byte-offset old))
                    => (trace b new (contextual-scan-state-byte-offset new)))
             (let (proof (prepare-contextual-scan-suffix old new))
               (let-values (((body old-next) (contextual-scanner-step a old 'command)))
                 (let-values (((body new-next) (contextual-scanner-step b new 'command)))
                   (let (rebound (contextual-scan-suffix-state proof old-next))
                     (check (contextual-scan-state-canonical rebound) => (contextual-scan-state-canonical new-next))
                     (check (trace b rebound (contextual-scan-state-byte-offset new))
                            => (trace b new-next (contextual-scan-state-byte-offset new))))))
               (for-each
                (lambda (outside)
                  (check (with-catch (lambda (condition) (error-message condition))
                           (lambda () (contextual-scan-suffix-state proof outside) #f))
                         => "checkpoint is outside the admitted scanner suffix"))
                (list (contextual-scanner-initial-state a) new)))
             ;; Each adversarial receipt changes lexical context with the same suffix.
             (for-each
              (lambda (replacement)
                (let ((changed (restore-contextual-scan-state
                                b (map (lambda (row)
                                         (if (eq? (car row) (car replacement)) replacement row)) receipt))))
                  (check (contextual-scan-state-converged? old changed) => #f)))
              '((active . ("B" #f #f)) (active . ("A" #t #f))
                (active . ("A" #f #t)) (expecting . plain) (mode . command)
                (pending . (("A" #f #f)))))
             (check (contextual-scan-state-converged?
                     old (advance (prepare-contextual-scanner plan "cat <<A\nβ\nA\n") 5)) => #f)
             (check (contextual-scan-state-converged?
                     old (advance (prepare-contextual-scanner
                                   (prepare-contextual-scanner-plan
                                    (deferred-scanner-ir 'convergence policy '("<<" "<<-")))
                                   "cat <<A\nα\nA\n") 5)) => #f))
           (let* ((pending (advance a 4))
                  (receipt (contextual-scan-state-canonical pending)))
             (check (contextual-scan-state-converged? pending (advance b 4)) => #t)
             (for-each
              (lambda (values)
                (let ((changed (restore-contextual-scan-state
                                a (map (lambda (row)
                                         (if (eq? (car row) 'pending) (cons 'pending values) row)) receipt))))
                  (check (contextual-scan-state-converged? pending changed) => #f)))
              '((("B" #f #f)) (("A" #t #f)) (("A" #f #t)))))
           (let* ((q (prepare-contextual-scanner plan "<<A <<B\nA\nB\n"))
                  (state (advance q 5))
                  (receipt (contextual-scan-state-canonical state))
                  (reordered (restore-contextual-scan-state
                              q (map (lambda (row)
                                       (if (eq? (car row) 'pending)
                                         (cons 'pending (reverse (cdr row))) row)) receipt))))
             (check (contextual-scan-state-converged? state reordered) => #f))))
       '(raw shell-quote-removal)))
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
    (test-case "checkpoint restore counts a Unicode source prefix and resumes its tail"
      (let* ((source "<<EOF\nα😀\nEOF\n")
             (scanner (deferred-scanner source 'hcl-deferred 'raw '("<<" "<<-"))))
        (let advance ((remaining 4) (state (contextual-scanner-initial-state scanner)))
          (if (positive? remaining)
            (let-values (((_token next) (contextual-scanner-step scanner state 'command)))
              (advance (- remaining 1) next))
            (let* ((receipt (contextual-scan-state-canonical state))
                   (restored (restore-contextual-scan-state scanner receipt)))
              (check (contextual-scan-state-byte-offset restored) => 13)
              (check (contextual-scan-state-canonical restored) => receipt)
              (let-values (((tail next) (contextual-scanner-step scanner restored 'command)))
                (check (token-lexeme tail) => "EOF\n")
                (check (token-start tail) => 13)
                (check (token-end tail) => 17)
                (check (contextual-scan-state-byte-offset next) => 17)))))))
    (test-case "scanner requests own source snapshots across declaration policies"
      (for-each
       (lambda (entry)
         (let* ((source (string-copy "<<EOF\nα\nEOF\n"))
                (scanner (deferred-scanner source (car entry) (cadr entry) '("<<")))
                (receipt (contextual-scan-state-canonical (contextual-scanner-initial-state scanner))))
           (string-set! source 2 #\X)
           (check (contextual-scan-state-canonical (contextual-scanner-initial-state scanner)) => receipt)
           (let-values (((tokens final) (scan-all scanner)))
             (check (map token-lexeme tokens) => '("<<" "EOF" "\n" "α\n" "EOF\n")))))
       '((hcl-owned raw) (bash-owned shell-quote-removal))))
    (test-case "published and restored checkpoints own pending and active markers"
      (for-each
       (lambda (entry)
         (let* ((scanner (deferred-scanner "<<EOF\nα\nEOF\n" (car entry) (cadr entry) '("<<")))
                (initial (contextual-scanner-initial-state scanner)))
           (let-values (((_open after-open) (contextual-scanner-step scanner initial 'command)))
             (let-values (((_marker pending) (contextual-scanner-step scanner after-open 'command)))
               (let* ((receipt (contextual-scan-state-canonical pending))
                      (expected (contextual-scan-state-canonical pending))
                      (restored (restore-contextual-scan-state scanner receipt)))
                 (string-set! (cdr (assq 'schema receipt)) 0 #\X)
                 (string-set! (cdr (assq 'scannerDigest receipt)) 0 #\X)
                 (string-set! (cdr (assq 'sourceDigest receipt)) 0 #\X)
                 (string-set! (caar (cdr (assq 'pending receipt))) 0 #\X)
                 (check (contextual-scan-state-canonical pending) => expected)
                 (check (contextual-scan-state-canonical restored) => expected)
                 (let-values (((_newline active) (contextual-scanner-step scanner restored 'command)))
                   (let* ((receipt (contextual-scan-state-canonical active))
                          (expected (contextual-scan-state-canonical active))
                          (restored (restore-contextual-scan-state scanner receipt)))
                     (string-set! (car (cdr (assq 'active receipt))) 0 #\X)
                     (check (contextual-scan-state-canonical active) => expected)
                     (check (contextual-scan-state-canonical restored) => expected)
                     (let-values (((body next) (contextual-scanner-step scanner restored 'command)))
                       (check (token-lexeme body) => "α\n")
                       (let-values (((end final) (contextual-scanner-step scanner next 'command)))
                         (check (token-lexeme end) => "EOF\n"))))))))))
       '((hcl-checkpoint-owned raw) (bash-checkpoint-owned shell-quote-removal))))
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
