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
                 prepare-contextual-scanner prepare-contextual-scanner-plan contextual-scanner-initial-state
                 contextual-scanner-step contextual-scan-state-byte-offset
                 contextual-scan-state-mode contextual-scan-state-with-mode
                 contextual-scan-state-canonical)
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

(def (with-cells ir cells)
  (let (body (map (lambda (row) (if (eq? (car row) 'cells) (cons 'cells cells) row))
                 (filter (lambda (row) (not (eq? (car row) 'digest))) ir)))
    (append body (list (cons 'digest (sha256-text
                                     (call-with-output-string (lambda (port) (write body port)))))))))

(def (plan-fixture)
  (let* ((literal (string-copy "if"))
         (role (make-contextual-role 'plan-fixture
                  (list (method 'literal 'any 'any 'literal 'keyword)
                        (method 'word 'any 'any 'word 'word))))
         (dispatch (compile-contextual-dispatch (list role) (list 'normal)
                      (list 'token) (list 'literal 'word)))
         (ir (compile-contextual-scanner
              (list (rule 'literal 'normal 'literal (list 'literal literal) 10)
                    (rule 'word 'normal 'word '(identifier) 0)) dispatch 'normal)))
    (values ir literal)))

(def contextual-scanner-test
  (test-suite "generic contextual scanner"
    (test-case "shared guarded matchers retain rule rank, action and independent request state"
      (let* ((role (make-contextual-role 'shared
                     (list (method 'normal 'normal 'any 'redirect 'redirect)
                           (method 'plain 'plain 'any 'redirect 'plain))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(normal plain) '(token) '(redirect)))
             (catalog (map (lambda (n) (string-append "padding" (number->string n))) (iota 128)))
             (ir (compile-contextual-scanner
                  (list (rule 'expect 'normal 'redirect '(unless-prefix ("<<<") () (literal "<<")) 20 '(expect-marker #f))
                        (rule 'lower 'normal 'redirect '(unless-prefix ("<<<") () (literal "<<")) 0)
                        (rule 'plain 'plain 'redirect '(unless-prefix ("<<<") () (literal "<<")) 0)
                        (rule 'catalog 'normal 'redirect (list 'literals catalog) -1))
                  dispatch 'normal))
             (plan (prepare-contextual-scanner-plan ir)))
        (for-each
         (lambda (source)
           (let ((a (prepare-contextual-scanner plan source))
                 (b (prepare-contextual-scanner plan source)))
             (let-values (((token next) (contextual-scanner-step
                                        a (contextual-scanner-initial-state a) 'token)))
               (check (token-kind token) => 'redirect)
               (check (cdr (assq 'expecting (contextual-scan-state-canonical next))) => 'plain))
             (let-values (((token next) (contextual-scanner-step b
                                        (contextual-scan-state-with-mode
                                         (contextual-scanner-initial-state b) 'plain) 'token)))
               (check (token-kind token) => 'plain)
               (check (token-lexeme token) => "<<")
               (check (cdr (assq 'expecting (contextual-scan-state-canonical next))) => #f))))
         (list "<<" (string-append "<<" (make-string 64 #\space))))))
    (test-case "prepared prefixes preserve full-row outcomes and checkpoint receipts"
      (for-each
       (lambda (source)
         (let-values (((linear ir) (bash-scanner source)))
           (let ((indexed (prepare-contextual-scanner (prepare-contextual-scanner-plan ir) source)))
             (let loop ((a (contextual-scanner-initial-state linear))
                        (b (contextual-scanner-initial-state indexed)))
               (let-values (((left next-left) (contextual-scanner-step linear a 'argument))
                            ((right next-right) (contextual-scanner-step indexed b 'argument)))
                 (check (and left (list (token-kind left) (token-lexeme left)
                                       (token-start left) (token-end left)))
                        => (and right (list (token-kind right) (token-lexeme right)
                                            (token-start right) (token-end right))))
                 (check (contextual-scan-state-canonical next-left)
                        => (contextual-scan-state-canonical next-right))
                 (when left (loop next-left next-right)))))))
       '("" "if αβ && name\n" "echo \"α\" |& next\r\n" "α $(echo x) >> file\n")))
    (test-case "scanner plans own IR and bind independent checkpoint identities"
      (let-values (((ir literal) (plan-fixture)))
        (let* ((digest (string-copy (cdr (assq 'digest ir))))
               (plan (prepare-contextual-scanner-plan ir))
               (a (prepare-contextual-scanner plan "if"))
               (receipt (contextual-scan-state-canonical (contextual-scanner-initial-state a))))
          (string-set! literal 0 #\x)
          (set-car! (cdr (assq 'modes ir)) 'foreign)
          (string-set! (cdr (assq 'digest ir)) 0 #\x)
          (string-set! (cdr (assq 'scannerDigest receipt)) 0 #\x)
          (check
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner ir "if") #f))
           => "contextual scanner requires compiled IR and source")
          (let* ((b (prepare-contextual-scanner plan "if"))
                 (initial (contextual-scanner-initial-state b))
                 (canonical (contextual-scan-state-canonical initial)))
            (check (cdr (assq 'scannerDigest canonical)) => digest)
            (let-values (((token next) (contextual-scanner-step b initial 'token)))
              (check (token-kind token) => 'keyword)
              (check (token-lexeme token) => "if")))
          (let (b (prepare-contextual-scanner plan "αβ"))
            (let-values (((token next) (contextual-scanner-step b (contextual-scanner-initial-state b) 'token)))
              (check (token-kind token) => 'word)
              (check (token-end token) => 4))))))
    (test-case "scanner plans reject cycles and callback data"
      (let (cycle (list 'cycle))
        (set-cdr! cycle cycle)
        (check (with-catch (lambda (condition) (error-message condition))
                 (lambda () (prepare-contextual-scanner-plan cycle) #f))
               => "cyclic contextual scanner plan IR"))
      (check (with-catch (lambda (condition) (error-message condition))
               (lambda () (prepare-contextual-scanner-plan (list (lambda () #f))) #f))
             => "contextual scanner plan requires closed IR data"))
    (test-case "dispatch indexing preserves negative sparse and duplicate cells"
      (let-values (((_scanner ir) (bash-scanner ";")))
        (let* ((cells (cdr (assq 'cells ir)))
               (target (find (lambda (cell) (equal? (take cell 3) '(command argument operator))) cells))
               (negative (map (lambda (cell) (if (eq? cell target)
                                                (append (take cell 3) '(#f)) cell)) cells))
               (sparse (filter (lambda (cell) (not (eq? cell target))) cells)))
          (for-each
           (lambda (rows)
             (let (scanner (prepare-contextual-scanner (with-cells ir rows) ";"))
               (check
                (with-catch (lambda (condition) (error-message condition))
                  (lambda () (contextual-scanner-step scanner
                               (contextual-scanner-initial-state scanner) 'argument) #f))
                => "missing contextual scanner dispatch")))
           (list negative sparse))
          (check
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner (with-cells ir (cons target negative)) ";") #f))
           => "duplicate contextual scanner dispatch cell")
          (let* ((wide (append cells
                        (map (lambda (n) (list 'command 'argument
                                          (string->symbol (string-append "extra" (number->string n)))
                                          '(unused))) (iota 9))))
                 (single (filter (lambda (cell) (eq? (cadr cell) 'argument)) cells))
                 (structural (cons (list '(foreign mode) 'argument 'foreign '(unused)) cells)))
            (for-each
             (lambda (rows)
               (let (scanner (prepare-contextual-scanner (with-cells ir rows) ";"))
                 (let-values (((token next) (contextual-scanner-step scanner
                                              (contextual-scanner-initial-state scanner) 'argument)))
                   (check (token-kind token) => 'operator)
                   (check (token-lexeme token) => ";"))))
             (list wide single structural))))))
    (test-case "position admission rejects undeclared identities even at EOF"
      (let-values (((scanner _ir) (bash-scanner "if")))
        (let (initial (contextual-scanner-initial-state scanner))
          (check
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (contextual-scanner-step scanner initial (gensym 'argument)) #f))
           => "invalid contextual scanner checkpoint or position")
          (let-values (((token next) (contextual-scanner-step scanner initial 'argument)))
            (check (token-kind token) => 'word)
            (check
             (with-catch (lambda (condition) (error-message condition))
               (lambda () (contextual-scanner-step scanner next 'undeclared) #f))
             => "invalid contextual scanner checkpoint or position")
            (let-values (((token final) (contextual-scanner-step scanner next 'command-start)))
              (check token => #f)
              (check (contextual-scan-state-byte-offset final) => 2))))))
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
    (test-case "literal catalogs preserve longest complete Unicode prefixes across modes"
      (let* ((role (make-contextual-role 'prefixes
                     (list (method 'literal 'any 'any 'literal 'literal))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(first second) '(token) '(literal)))
             (catalog (append '("α" "αβ" "αβγ")
                              (map (lambda (n) (string-append "padding" (number->string n)))
                                   (iota 128))))
             (ir (compile-contextual-scanner
                  (list (rule 'first 'first 'literal (list 'literals catalog) 0)
                        (rule 'second 'second 'literal (list 'literals catalog) 0))
                  dispatch 'first))
             (scanner (prepare-contextual-scanner (prepare-contextual-scanner-plan ir)
                        (string-append "αβαβx" (make-string 64 #\x)))))
        (let-values (((a next) (contextual-scanner-step
                                scanner (contextual-scanner-initial-state scanner) 'token)))
          (check (token-lexeme a) => "αβ")
          (check (list (token-start a) (token-end a)) => '(0 4))
          (let-values (((b last) (contextual-scanner-step
                                  scanner (contextual-scan-state-with-mode next 'second) 'token)))
            (check (token-lexeme b) => "αβ")
            (check (list (token-start b) (token-end b)) => '(4 8))
            (check (contextual-scan-state-byte-offset last) => 8)
            (check
             (with-catch (lambda (condition) (error-message condition))
               (lambda () (contextual-scanner-step scanner last 'token) #f))
             => "contextual scanner has no match")))))
    (test-case "closed guards retain Unicode exceptions and prefix rejection"
      (let* ((role (make-contextual-role 'guarded
                     (list (method 'word 'any 'any 'word 'word)
                           (method 'literal 'any 'any 'literal 'literal))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(normal) '(token) '(word literal)))
             (ir (compile-contextual-scanner
                  (list (rule 'guarded-word 'normal 'word
                              '(unless-prefix ("ab" "λ") ("abc" "λx") (identifier)) 10)
                        (rule 'fallback 'normal 'literal '(literals ("ab" "λ")) 0))
                  dispatch 'normal)))
        (for-each
         (lambda (input)
           (for-each
            (lambda (recipe)
              (let (scanner (prepare-contextual-scanner recipe (car input)))
                (let-values (((token next) (contextual-scanner-step
                                           scanner (contextual-scanner-initial-state scanner) 'token)))
                  (check (list (token-kind token) (token-lexeme token) (token-end token))
                         => (cdr input)))))
            (list ir (prepare-contextual-scanner-plan ir))))
         '(("abc" word "abc" 3) ("abx" literal "ab" 2)
           ("λx" word "λx" 3) ("λz" literal "λ" 2)
           ("z" word "z" 1)))))
    (test-case "candidate selection preserves length before rank in either declaration order"
      (let* ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long))))
        (for-each
         (lambda (rows)
           (let (ir (compile-contextual-scanner (map (lambda (row) (apply rule row)) rows) dispatch 'normal))
             (for-each
              (lambda (recipe)
                (let (scanner (prepare-contextual-scanner recipe "λx"))
                  (let-values (((token next) (contextual-scanner-step scanner
                                             (contextual-scanner-initial-state scanner) 'token)))
                    (check (list (token-kind token) (token-lexeme token) (token-end token)
                                 (contextual-scan-state-byte-offset next)) => '(long "λx" 3 3)))))
              (list ir (prepare-contextual-scanner-plan ir)))))
         '(((short normal short (literal "λ") 100) (long normal long (literal "λx") 0))
           ((long normal long (literal "λx") 0) (short normal short (literal "λ") 100))
           ((short normal short (literal "λx") 0) (long normal long (literal "λx") 1))
           ((long normal long (literal "λx") 1) (short normal short (literal "λx") 0))))))
    (test-case "candidate conflicts remain eager even before a longer later match"
      (let* ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long)))
             (ir (compile-contextual-scanner
                  (list (rule 'first 'normal 'short '(literal "λ") 0)
                        (rule 'conflict 'normal 'long '(literals ("λ" "Ω")) 0)
                        (rule 'later 'normal 'long '(literal "λx") 0)) dispatch 'normal)))
        (for-each
         (lambda (recipe)
           (let (scanner (prepare-contextual-scanner recipe "λx"))
             (check (with-catch error-message
                      (lambda () (contextual-scanner-step scanner
                                   (contextual-scanner-initial-state scanner) 'token) #f))
                    => "ambiguous contextual scanner match")))
         (list ir (prepare-contextual-scanner-plan ir)))))
    (test-case "equivalent ties preserve the first rule in conflict diagnostics"
      (let* ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long)))
             (ir (compile-contextual-scanner
                  (list (rule 'first 'normal 'short '(literal "λ") 0)
                        (rule 'equivalent 'normal 'short '(literals ("λ" "Ω")) 0)
                        (rule 'conflict 'normal 'long '(unless-prefix ("Ω") () (literal "λ")) 0))
                  dispatch 'normal)))
        (for-each
         (lambda (recipe)
           (let (scanner (prepare-contextual-scanner recipe "λ"))
             (check (with-catch error-irritants
                      (lambda () (contextual-scanner-step scanner
                                   (contextual-scanner-initial-state scanner) 'token) #f))
                    => '(first conflict))))
         (list ir (prepare-contextual-scanner-plan ir)))))
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
             (scanner (prepare-contextual-scanner (prepare-contextual-scanner-plan ir) "<<")))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (contextual-scanner-step
             scanner (contextual-scanner-initial-state scanner) 'command)
            #f))
         => "ambiguous contextual scanner match")))
    (test-case "input IR cannot admit the private prepared matchers under guards"
      (for-each (lambda (matcher)
      (let-values (((_scanner ir) (bash-scanner "if")))
        (let* ((body
                (map (lambda (row)
                       (if (eq? (car row) 'rules)
                         (cons 'rules
                               (map (lambda (rule)
                                      (list (car rule) (cadr rule) (caddr rule)
                                            matcher
                                            (list-ref rule 4) (list-ref rule 5)))
                                    (cdr row))) row))
                     (filter (lambda (row) (not (eq? (car row) 'digest))) ir)))
               (altered (append body
                                (list (cons 'digest
                                            (sha256-text (call-with-output-string
                                                          (lambda (port) (write body port)))))))))
          (check
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner altered "if") #f))
           => "private contextual scanner matcher in input IR"))))
       '((literal-trie 42) (prepared-region 42)
         (unless-prefix ("#") () (literal-trie 42))
         (unless-prefix ("#") () (prepared-region 42)))))
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
