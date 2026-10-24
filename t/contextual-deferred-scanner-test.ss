(import :gerbil-parser/language-test-support)
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


(def (method name mode position form result)
  (make-contextual-method name mode position form result))

(def (rule name mode form matcher rank action)
  (make-contextual-scan-rule name mode form matcher rank action))

(def bash-operators
  '(";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
    "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")"
    "<" ">"))

(def (deferred-scanner-ir owner marker-policy operators (separator #f))
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
                (rule 'end 'here-body 'end
                      (if separator (list 'marker-line-at (string separator))
                        '(marker-line)) 10
                      '(finish-marker command here-body))
                (rule 'body 'here-body 'body
                      (if separator (list 'body-line-at (string separator))
                        '(body-line)) 0 'keep)))
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

(deflanguage-parser-tests contextual-deferred-scanner-test "generic deferred delimiter scanner"
  (engine)
    (scenario "prefix replay reaches source-owned HCL and Bash delimiter states"
      (each (policy) ('(raw shell-quote-removal))
         (sequential ((plan (prepare-contextual-scanner-plan
                       (deferred-scanner-ir 'prefix-replay policy '("<<" "<<-" "<<<"))))
                (old (prepare-contextual-scanner plan "猫 <<A\nα\nA\n"))
                (new (prepare-contextual-scanner plan "猫 <<A\nβ\nA\n")))
(locals ((advance (lambda ( scanner count)
             (let loop ((state (contextual-scanner-initial-state scanner)) (n count))
               (if (zero? n) state
                 (let-values (((token next) (contextual-scanner-step scanner state 'command)))
                   (loop next (- n 1))))))))

           (each (count) ('(0 4 5))
              (sequential ((checkpoint (advance old count))
                     (receipt (contextual-scan-state-canonical checkpoint))
                     (replayed (contextual-scanner-replay-prefix new checkpoint (make-list count 'command))))
                (equal (contextual-scan-state-canonical replayed)
                        (contextual-scan-state-canonical (advance new count)))
                (equal (contextual-scan-state-canonical checkpoint)  receipt)
                (raises (contextual-scanner-step new checkpoint 'command) true)
                (scope-values (((token next) (contextual-scanner-step new replayed 'command)))
                  (equal (and token #t)  #t))))
           (scope ((checkpoint (advance old 5)))
             (raises (contextual-scanner-replay-prefix new checkpoint '(command)) true)
             (raises (contextual-scanner-replay-prefix new checkpoint (make-list 6 'command)) true)
             (raises (contextual-scanner-replay-prefix new checkpoint '(unknown)) true)
             (raises
              (contextual-scanner-replay-prefix new (contextual-scan-state-with-mode checkpoint 'command)
                                               (make-list 5 'command)) true))
           (each (sources) ('(("cat" "catx") ("<<A" "<<-A") ("\"x\"" "\"x\"tail") ("猫 " "犬 ") ("cat " "ca")))
              (sequential ((a (prepare-contextual-scanner plan (car sources)))
                     (b (prepare-contextual-scanner plan (cadr sources)))
                     (checkpoint (advance a 1)))
                ;; The prefix bytes match, but actual longest-match/EOF/quote decisions change.
                (raises (contextual-scanner-replay-prefix b checkpoint '(command)) true)))
           (scope ((foreign (prepare-contextual-scanner
                           (prepare-contextual-scanner-plan
                            (deferred-scanner-ir 'prefix-replay policy '("<<" "<<-" "<<<")))
                           "猫 <<A\nβ\nA\n")))
             (raises (contextual-scanner-replay-prefix foreign (advance old 5)
                                                              (make-list 5 'command)) true))))))
    (scenario "edited HCL and Bash prefixes converge only with the complete lexical context"
      (each (policy) ('(raw shell-quote-removal))
         (sequential ((plan (prepare-contextual-scanner-plan
                       (deferred-scanner-ir 'convergence policy '("<<" "<<-"))))
                (a (prepare-contextual-scanner plan "cat <<A\nα\nA\n"))
                (b (prepare-contextual-scanner plan "猫猫 <<A\nα\nA\n")))
(locals ((advance (lambda ( scanner count)
             (let loop ((state (contextual-scanner-initial-state scanner)) (n count))
               (if (zero? n) state
                 (let-values (((token next) (contextual-scanner-step scanner state 'command)))
                   (loop next (- n 1)))))))
(trace (lambda ( scanner state origin)
             (let-values (((token next) (contextual-scanner-step scanner state 'command)))
               (if token
                 (cons (list (token-kind token) (token-lexeme token)
                             (- (token-start token) origin) (- (token-end token) origin))
                       (trace scanner next origin)) '())))))


           (sequential ((old (advance a 5)) (new (advance b 5))
                  (receipt (contextual-scan-state-canonical new)))
             (equal (contextual-scan-state-converged? old new)  #t)
             (equal (trace a old (contextual-scan-state-byte-offset old))
                     (trace b new (contextual-scan-state-byte-offset new)))
             (scope ((proof (prepare-contextual-scan-suffix old new)))
               (scope-values (((body old-next) (contextual-scanner-step a old 'command)))
                 (scope-values (((body new-next) (contextual-scanner-step b new 'command)))
                   (scope ((rebound (contextual-scan-suffix-state proof old-next)))
                     (equal (contextual-scan-state-canonical rebound)  (contextual-scan-state-canonical new-next))
                     (equal (trace b rebound (contextual-scan-state-byte-offset new))
                             (trace b new-next (contextual-scan-state-byte-offset new))))))
               (each (outside) ((list (contextual-scanner-initial-state a) new))
                  (equal (with-catch (lambda (condition) (error-message condition))
                           (lambda () (contextual-scan-suffix-state proof outside) #f))
                          "checkpoint is outside the admitted scanner suffix")))
             ;; Each adversarial receipt changes lexical context with the same suffix.
             (each (replacement) ('((active . ("B" #f #f)) (active . ("A" #t #f))
                (active . ("A" #f #t)) (expecting . plain) (mode . command)
                (pending . (("A" #f #f)))))
                (scope ((changed (restore-contextual-scan-state
                                b (map (lambda (row)
                                         (if (eq? (car row) (car replacement)) replacement row)) receipt))))
                  (equal (contextual-scan-state-converged? old changed)  #f)))
             (equal (contextual-scan-state-converged?
                     old (advance (prepare-contextual-scanner plan "cat <<A\nβ\nA\n") 5))  #f)
             (equal (contextual-scan-state-converged?
                     old (advance (prepare-contextual-scanner
                                   (prepare-contextual-scanner-plan
                                    (deferred-scanner-ir 'convergence policy '("<<" "<<-")))
                                   "cat <<A\nα\nA\n") 5))  #f))
           (sequential ((pending (advance a 4))
                  (receipt (contextual-scan-state-canonical pending)))
             (equal (contextual-scan-state-converged? pending (advance b 4))  #t)
             (each (values) ('((("B" #f #f)) (("A" #t #f)) (("A" #f #t))))
                (scope ((changed (restore-contextual-scan-state
                                a (map (lambda (row)
                                         (if (eq? (car row) 'pending) (cons 'pending values) row)) receipt))))
                  (equal (contextual-scan-state-converged? pending changed)  #f))))
           (sequential ((q (prepare-contextual-scanner plan "<<A <<B\nA\nB\n"))
                  (state (advance q 5))
                  (receipt (contextual-scan-state-canonical state))
                  (reordered (restore-contextual-scan-state
                              q (map (lambda (row)
                                       (if (eq? (car row) 'pending)
                                         (cons 'pending (reverse (cdr row))) row)) receipt))))
             (equal (contextual-scan-state-converged? state reordered)  #f))))))
    (scenario "shared scanner plans keep delimiter queues source-local"
      (sequential ((ir (deferred-scanner-ir 'plan-queues 'raw '("<<")))
             (plan (prepare-contextual-scanner-plan ir))
             (a (prepare-contextual-scanner plan "<<A\nα\nA\n"))
             (b (prepare-contextual-scanner plan "<<B\nβ\nB\n")))
(locals ((queued (lambda ( scanner)
          (let-values (((open next) (contextual-scanner-step scanner (contextual-scanner-initial-state scanner) 'command)))
            (let-values (((word next) (contextual-scanner-step scanner next 'command))) next)))))

        (equal (cdr (assq 'pending (contextual-scan-state-canonical (queued a))))  '(("A" #f #f)))
        (equal (cdr (assq 'pending (contextual-scan-state-canonical (contextual-scanner-initial-state b))))  '())
        (equal (cdr (assq 'pending (contextual-scan-state-canonical (queued b))))  '(("B" #f #f)))
        (scope-values (((tokens final) (scan-all a)))
          (equal (map token-lexeme tokens)  '("<<" "A" "\n" "α\n" "A\n")))
        (scope-values (((tokens final) (scan-all b)))
          (equal (map token-lexeme tokens)  '("<<" "B" "\n" "β\n" "B\n"))))))
    (scenario "queued checkpoints preserve FIFO order across independent futures"
      (sequential ((source "<<A <<B <<C\nα\nA\nβ\nB\nγ\nC\n")
             (scanner (deferred-scanner source 'queue-checkpoint 'raw '("<<")))
             (initial (contextual-scanner-initial-state scanner)))
(locals ((advance (lambda ( state count)
          (if (zero? count) state
            (let-values (((_token next)
                          (contextual-scanner-step scanner state 'command)))
              (advance next (- count 1))))))
(suffix (lambda ( state)
          (let loop ((state state) (trace '()))
            (let-values (((token next)
                          (contextual-scanner-step scanner state 'command)))
              (if token
                (loop next
                      (cons (list (token-kind token) (token-lexeme token)
                                  (contextual-scan-state-canonical next)) trace))
                (reverse trace)))))))


        (sequential ((after-a (advance initial 2))
               (receipt (contextual-scan-state-canonical after-a))
               (restored (restore-contextual-scan-state scanner receipt))
               (after-b (advance restored 3))
               (second (contextual-scan-state-canonical after-b)))
          (equal (cdr (assq 'pending second))  '(("A" #f #f) ("B" #f #f)))
          (equal (suffix after-a)  (suffix restored))
          (equal (suffix after-b)
                  (suffix (restore-contextual-scan-state scanner second)))
          (equal (map cadr (filter (lambda (row) (eq? (car row) 'here-end))
                                  (suffix restored)))
                  '("A\n" "B\n" "C\n"))
          (equal (contextual-scan-state-canonical after-a)  receipt)))))
    (scenario "Bash here bodies preserve marker order and CRLF bytes"
      (sequential ((source "cat <<'A' <<-B\r\n$x α\r\nA\r\n\tβ $x\r\n\tB\r\n")
             (scanner
              (deferred-scanner source 'bash-deferred
                                'shell-quote-removal bash-operators)))
        (scope-values (((tokens final) (scan-all scanner)))
          (equal (map token-kind
                      (filter (lambda (token)
                                (memq (token-kind token)
                                      '(here-body here-end)))
                              tokens))
                  '(here-body here-end here-body here-end))
          (equal (map token-lexeme
                      (filter (lambda (token)
                                (eq? (token-kind token) 'here-end))
                              tokens))
                  '("A\r\n" "\tB\r\n"))
          (equal (contextual-scan-state-mode final)  'command)
          (equal (contextual-scan-state-byte-offset final)
                  (u8vector-length (string->utf8 source)))
          (equal (cdr (assq 'pending (contextual-scan-state-canonical final)))
                  '())
          (equal (cdr (assq 'active (contextual-scan-state-canonical final)))
                  #f))))
    (scenario "marker slices reject equal-prefix and equal-suffix body lines"
      (each (separator) ('(#f #\newline #\return))
         (sequential ((line-end (string (or separator #\newline)))
                (source (string-append "<<-猫" line-end
                                       "\t猫x" line-end "\tx猫" line-end
                                       "\t猫猫" line-end "\t猫"))
                (plan (prepare-contextual-scanner-plan
                       (deferred-scanner-ir 'slice-marker 'raw
                                            '("<<" "<<-") separator)))
                (scanner (prepare-contextual-scanner plan source)))
           (scope-values (((tokens final) (scan-all scanner)))
             (equal (map token-kind tokens)
                     '(here-open word newline here-body here-body here-body here-end))
             (equal (map token-lexeme (list-tail tokens 3))
                     (list (string-append "\t猫x" line-end)
                              (string-append "\tx猫" line-end)
                              (string-append "\t猫猫" line-end) "\t猫"))
             (equal (contextual-scan-state-mode final)  'command)
             (equal (contextual-scan-state-byte-offset final)
                     (u8vector-length (string->utf8 source)))))))
    (scenario "HCL raw marker uses the same deferred scanner"
      (sequential ((source "<<EOF\nα\nEOF")
             (scanner (deferred-scanner source 'hcl-deferred
                                        'raw '("<<" "<<-"))))
        (scope-values (((tokens final) (scan-all scanner)))
          (equal (map token-kind tokens)
                  '(here-open word newline here-body here-end))
          (equal (token-lexeme (list-ref tokens 3))  "α\n")
          (equal (token-lexeme (list-ref tokens 4))  "EOF")
          (equal (contextual-scan-state-byte-offset final)
                  (u8vector-length (string->utf8 source))))))
    (property "an unfinished marker fails at EOF"
      (bindings (scanner
            (deferred-scanner "cat <<EOF\nbody\n" 'bash-deferred
                              'shell-quote-removal bash-operators)))
      (equal
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda () (scan-all scanner) #f))
          "unfinished contextual scanner obligation"))
    (scenario "shell marker quote removal preserves ordinary backslashes"
      (sequential ((source "cat <<\"\\q\"\npayload\n\\q\n")
             (scanner (deferred-scanner source 'bash-deferred
                                        'shell-quote-removal bash-operators)))
        (scope-values (((tokens final) (scan-all scanner)))
          (equal (apply string-append (map token-lexeme tokens))  source)
          (equal (map token-lexeme
                      (filter (lambda (token) (eq? (token-kind token) 'here-end))
                              tokens))
                  '("\\q\n")))))
    (scenario "empty quoted shell marker closes on the blank line"
      (sequential ((source "cat <<''\npayload\n\n")
             (scanner (deferred-scanner source 'bash-deferred
                                        'shell-quote-removal bash-operators)))
        (scope-values (((tokens final) (scan-all scanner)))
          (equal (apply string-append (map token-lexeme tokens))  source)
          (equal (map token-lexeme
                      (filter (lambda (token) (eq? (token-kind token) 'here-end))
                              tokens))
                  '("\n")))))
    (scenario "checkpoint restore counts a Unicode source prefix and resumes its tail"
      (sequential ((source "<<EOF\nα😀\nEOF\n")
             (scanner (deferred-scanner source 'hcl-deferred 'raw '("<<" "<<-"))))
        (iterate advance ((remaining 4) (state (contextual-scanner-initial-state scanner)))
          (branch (positive? remaining)
            (scope-values (((_token next) (contextual-scanner-step scanner state 'command)))
              (continue advance (- remaining 1) next))
            (sequential ((receipt (contextual-scan-state-canonical state))
                   (restored (restore-contextual-scan-state scanner receipt)))
              (equal (contextual-scan-state-byte-offset restored)  13)
              (equal (contextual-scan-state-canonical restored)  receipt)
              (scope-values (((tail next) (contextual-scanner-step scanner restored 'command)))
                (equal (token-lexeme tail)  "EOF\n")
                (equal (token-start tail)  13)
                (equal (token-end tail)  17)
                (equal (contextual-scan-state-byte-offset next)  17)))))))
    (scenario "scanner requests own source snapshots across declaration policies"
      (each (entry) ('((hcl-owned raw) (bash-owned shell-quote-removal)))
         (sequential ((source (string-copy "<<EOF\nα\nEOF\n"))
                (scanner (deferred-scanner source (car entry) (cadr entry) '("<<")))
                (receipt (contextual-scan-state-canonical (contextual-scanner-initial-state scanner))))
           (act (string-set! source 2 #\X))
           (equal (contextual-scan-state-canonical (contextual-scanner-initial-state scanner))  receipt)
           (scope-values (((tokens final) (scan-all scanner)))
             (equal (map token-lexeme tokens)  '("<<" "EOF" "\n" "α\n" "EOF\n"))))))
    (scenario "published and restored checkpoints own pending and active markers"
      (each (entry) ('((hcl-checkpoint-owned raw) (bash-checkpoint-owned shell-quote-removal)))
         (sequential ((scanner (deferred-scanner "<<EOF\nα\nEOF\n" (car entry) (cadr entry) '("<<")))
                (initial (contextual-scanner-initial-state scanner)))
           (scope-values (((_open after-open) (contextual-scanner-step scanner initial 'command)))
             (scope-values (((_marker pending) (contextual-scanner-step scanner after-open 'command)))
               (sequential ((receipt (contextual-scan-state-canonical pending))
                      (expected (contextual-scan-state-canonical pending))
                      (restored (restore-contextual-scan-state scanner receipt)))
                 (act (string-set! (cdr (assq 'schema receipt)) 0 #\X))
                 (act (string-set! (cdr (assq 'scannerDigest receipt)) 0 #\X))
                 (act (string-set! (cdr (assq 'sourceDigest receipt)) 0 #\X))
                 (act (string-set! (caar (cdr (assq 'pending receipt))) 0 #\X))
                 (equal (contextual-scan-state-canonical pending)  expected)
                 (equal (contextual-scan-state-canonical restored)  expected)
                 (scope-values (((_newline active) (contextual-scanner-step scanner restored 'command)))
                   (sequential ((receipt (contextual-scan-state-canonical active))
                          (expected (contextual-scan-state-canonical active))
                          (restored (restore-contextual-scan-state scanner receipt)))
                     (act (string-set! (car (cdr (assq 'active receipt))) 0 #\X))
                     (equal (contextual-scan-state-canonical active)  expected)
                     (equal (contextual-scan-state-canonical restored)  expected)
                     (scope-values (((body next) (contextual-scanner-step scanner restored 'command)))
                       (equal (token-lexeme body)  "α\n")
                       (scope-values (((end final) (contextual-scanner-step scanner next 'command)))
                         (equal (token-lexeme end)  "EOF\n")))))))))))
    (scenario "checkpoint restores only for the same source and machine"
      (sequential ((source "<<EOF\nα\nEOF\n")
             (scanner (deferred-scanner source 'hcl-deferred
                                        'raw '("<<" "<<-")))
             (initial (contextual-scanner-initial-state scanner)))
        (scope-values (((_open after-open)
                      (contextual-scanner-step scanner initial 'command)))
          (scope-values (((_marker after-marker)
                        (contextual-scanner-step scanner after-open 'command)))
            (scope-values (((_newline active)
                          (contextual-scanner-step scanner after-marker
                                                   'command)))
              (sequential ((receipt (contextual-scan-state-canonical active))
                     (restored
                      (restore-contextual-scan-state scanner receipt))
                     (other-source
                      (deferred-scanner "<<EOF\nβ\nEOF\n" 'hcl-deferred
                                        'raw '("<<" "<<-"))))
                (equal (contextual-scan-state-canonical restored)
                        receipt)
                (equal
                 (with-catch
                  (lambda (condition) (error-message condition))
                  (lambda ()
                    (restore-contextual-scan-state other-source receipt)
                    #f))
                  "contextual scanner checkpoint identity mismatch")
                (equal
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
                  "invalid contextual scanner checkpoint"))))))))
