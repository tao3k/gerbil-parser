;;; -*- Gerbil -*-
;;; Two language declaration shapes execute through one engine scanner.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role
                 make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-dispatch
                 compile-contextual-dispatch)
        (only-in :gerbil-parser/src/compiler/contextual-scanner-ir
                 compile-contextual-scanner contextual-scanner-ir-ref)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 prepare-contextual-scanner contextual-scanner-initial-state
                 contextual-scanner-step contextual-scan-state-byte-offset
                 contextual-scan-state-mode contextual-scan-state-with-mode)
        (only-in :gerbil-parser/src/runtime/token
                 token-end token-kind token-lexeme token-start)
        (only-in :gerbil-parser/src/runtime/identity sha256-text))
(export contextual-scanner-test)

(def (method name mode position form result)
  (make-contextual-method name mode position form result))

(def (rule name mode form matcher rank (action 'keep))
  (make-contextual-scan-rule name mode form matcher rank action))

(def bash-operators
  '(";;&" "&>>" "<<-" "<<<" "&&" "||" "|&" ";;" ";&"
    "<<" ">>" "<>" "<&" ">&" ">|" "&>" ";" "&" "|" "(" ")"
    "<" ">"))

(def bash-pairs
  (list (list "${" #\{ #\})
        (list "$(" #\( #\))
        (list "<(" #\( #\))
        (list ">(" #\( #\))))

(def (bash-scanner source)
  (let* ((role
          (make-contextual-role
           'bash-lexical
           (list (method 'operator 'any 'any 'operator 'operator)
                 (method 'word 'any 'any 'word 'word)
                 (method 'reserved 'command 'command-start
                         'keyword 'reserved-word)
                 (method 'keyword-argument 'any 'any 'keyword 'word)
                 (method 'space 'any 'any 'space 'horizontal-whitespace)
                 (method 'newline 'any 'any 'newline 'newline))))
         (dispatch
          (compile-contextual-dispatch
           (list role) '(command) '(command-start argument)
           '(operator word keyword space newline)))
         (rules
          (list (rule 'operator 'command 'operator
                      (list 'literals bash-operators) 20)
                (rule 'reserved-if 'command 'keyword '(literal "if") 10)
                (rule 'word 'command 'word
                      (list 'balanced-word bash-operators
                            '("'" "\"" "`") bash-pairs) 0)
                (rule 'space 'command 'space '(horizontal-whitespace+) 0)
                (rule 'newline 'command 'newline '(newline) 0)))
         (ir (compile-contextual-scanner rules dispatch 'command)))
    (values (prepare-contextual-scanner ir source) ir)))

(def (hcl-template-scanner source)
  (let* ((role
          (make-contextual-role
           'hcl-template
           (list (method 'open 'any 'any 'open 'interpolation-open)
                 (method 'text 'any 'any 'text 'template-text)
                 (method 'space 'any 'any 'space 'template-text)
                 (method 'identifier 'any 'any 'identifier 'identifier)
                 (method 'close 'any 'any 'close 'interpolation-close))))
         (dispatch
          (compile-contextual-dispatch
           (list role) '(template expression) '(text expression)
           '(open text space identifier close)))
         (rules
          (list (rule 'open 'template 'open '(literal "${") 10)
                (rule 'text 'template 'text
                      '(balanced-word ("${" "}") ("\"") ()) 0)
                (rule 'space 'template 'space '(horizontal-whitespace+) 0)
                (rule 'identifier 'expression 'identifier '(identifier) 0)
                (rule 'close 'expression 'close '(literal "}") 0)))
         (ir (compile-contextual-scanner rules dispatch 'template)))
    (values (prepare-contextual-scanner ir source) ir)))

(def contextual-scanner-test
  (test-suite "generic contextual scanner"
    (test-case "Bash nested word scans without a language callback"
      (let-values (((scanner ir)
                    (bash-scanner "if printf %s \"${x:-$(printf '%s' '}')}\"\n")))
        (let loop ((state (contextual-scanner-initial-state scanner))
                   (position 'command-start)
                   (tokens '()))
          (let-values (((token next)
                        (contextual-scanner-step scanner state position)))
            (if token
              (loop next 'argument (cons token tokens))
              (let (found (reverse tokens))
                (check (map token-kind found)
                       => '(reserved-word horizontal-whitespace word
                             horizontal-whitespace word
                             horizontal-whitespace word newline))
                (check (token-lexeme (list-ref found 6))
                       => "\"${x:-$(printf '%s' '}')}\"")
                (check (contextual-scan-state-byte-offset next)
                       => (u8vector-length
                           (string->utf8
                            "if printf %s \"${x:-$(printf '%s' '}')}\"\n")))
                (check (string? (contextual-scanner-ir-ref ir 'digest))
                       => #t)))))))
    (test-case "HCL template and expression modes share the executor"
      (let-values (((scanner _ir) (hcl-template-scanner "hello ${name} α")))
        (let* ((initial (contextual-scanner-initial-state scanner))
               (tokens '()))
          (def (take state position)
            (let-values (((token next)
                          (contextual-scanner-step scanner state position)))
              (set! tokens (append tokens (list token)))
              next))
          (let* ((a (take initial 'text))
                 (b (take a 'text))
                 (c (take b 'text))
                 (d (take (contextual-scan-state-with-mode c 'expression)
                          'expression))
                 (e (take d 'expression))
                 (f (take (contextual-scan-state-with-mode e 'template) 'text))
                 (g (take f 'text)))
            (check (map token-kind tokens)
                   => '(template-text template-text interpolation-open
                         identifier interpolation-close template-text
                         template-text))
            (check (contextual-scan-state-mode g) => 'template)
            (check (token-start (list-ref tokens 6)) => (token-end (list-ref tokens 5)))
            (check (contextual-scan-state-byte-offset g)
                   => (u8vector-length (string->utf8 "hello ${name} α")))))))
    (test-case "equal token forms do not hide conflicting scanner actions"
      (let* ((role (make-contextual-role
                    'redirect
                    (list (method 'redirect 'any 'any 'redirect 'redirect))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(command) '(command) '(redirect)))
             (ir (compile-contextual-scanner
                  (list (rule 'open 'command 'redirect '(literal "<<") 0
                              '(expect-marker #f))
                        (rule 'plain 'command 'redirect
                              '(literals ("<<")) 0 'keep))
                  dispatch 'command))
             (scanner (prepare-contextual-scanner ir "<<")))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (contextual-scanner-step
             scanner (contextual-scanner-initial-state scanner) 'command)
            #f))
         => "ambiguous contextual scanner match")))
    (test-case "a self-consistent digest cannot admit a foreign opcode contract"
      (let-values (((_scanner ir) (bash-scanner "if")))
        (let* ((body
                (map (lambda (row)
                       (if (eq? (car row) 'opcode-contract)
                         (cons 'opcode-contract "foreign-opcodes.v1") row))
                     (filter (lambda (row) (not (eq? (car row) 'digest))) ir)))
               (altered
                (append body
                        (list (cons 'digest
                                    (sha256-text
                                     (call-with-output-string
                                      (lambda (port) (write body port)))))))))
          (check
           (with-catch
            (lambda (condition) (error-message condition))
            (lambda () (prepare-contextual-scanner altered "if") #f))
           => "contextual scanner requires compiled IR and source"))))
    (test-case "compiler rejects altered dispatch and duplicate scan candidates"
      (let* ((role (make-contextual-role
                    'validation
                    (list (method 'first 'any 'any 'first 'first)
                          (method 'second 'any 'any 'second 'second))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(normal) '(start) '(first second)))
             (first (rule 'first 'normal 'first '(literal "x") 0))
             (second (rule 'second 'normal 'second '(literal "x") 0))
             (altered
              (map (lambda (row)
                     (if (eq? (car row) 'cells)
                       (cons 'cells '()) row))
                   dispatch)))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (compile-contextual-scanner (list first second)
                                        dispatch 'normal)
            #f))
         => "duplicate contextual scan candidate")
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (compile-contextual-scanner (list first) altered 'normal)
            #f))
         => "invalid contextual scanner declaration")))))
