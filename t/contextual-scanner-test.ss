(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Two language declaration shapes execute through one engine scanner.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/grammar/contextual-objects
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

(deflanguage-parser-tests contextual-scanner-test "generic contextual scanner"
  (engine)
    (scenario "shared guarded matchers retain rule rank, action and independent request state"
      (sequential ((role (make-contextual-role 'shared
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
        (each (source) ((list "<<" (string-append "<<" (make-string 64 #\space))))
           (scope ((a (prepare-contextual-scanner plan source))
                 (b (prepare-contextual-scanner plan source)))
             (scope-values (((token next) (contextual-scanner-step
                                        a (contextual-scanner-initial-state a) 'token)))
               (equal (token-kind token)  'redirect)
               (equal (cdr (assq 'expecting (contextual-scan-state-canonical next)))  'plain))
             (scope-values (((token next) (contextual-scanner-step b
                                        (contextual-scan-state-with-mode
                                         (contextual-scanner-initial-state b) 'plain) 'token)))
               (equal (token-kind token)  'plain)
               (equal (token-lexeme token)  "<<")
               (equal (cdr (assq 'expecting (contextual-scan-state-canonical next)))  #f))))))
    (scenario "prepared prefixes preserve full-row outcomes and checkpoint receipts"
      (each (source) ('("" "if αβ && name\n" "echo \"α\" |& next\r\n" "α $(echo x) >> file\n"))
         (scope-values (((linear ir) (bash-scanner source)))
           (scope ((indexed (prepare-contextual-scanner (prepare-contextual-scanner-plan ir) source)))
             (iterate loop ((a (contextual-scanner-initial-state linear))
                        (b (contextual-scanner-initial-state indexed)))
               (scope-values (((left next-left) (contextual-scanner-step linear a 'argument))
                            ((right next-right) (contextual-scanner-step indexed b 'argument)))
                 (equal (and left (list (token-kind left) (token-lexeme left)
                                       (token-start left) (token-end left)))
                         (and right (list (token-kind right) (token-lexeme right)
                                            (token-start right) (token-end right))))
                 (equal (contextual-scan-state-canonical next-left)
                         (contextual-scan-state-canonical next-right))
                 (when left (continue loop next-left next-right))))))))
    (scenario "scanner plans own IR and bind independent checkpoint identities"
      (scope-values (((ir literal) (plan-fixture)))
        (sequential ((digest (string-copy (cdr (assq 'digest ir))))
               (plan (prepare-contextual-scanner-plan ir))
               (a (prepare-contextual-scanner plan "if"))
               (receipt (contextual-scan-state-canonical (contextual-scanner-initial-state a))))
          (act (string-set! literal 0 #\x))
          (act (set-car! (cdr (assq 'modes ir)) 'foreign))
          (act (string-set! (cdr (assq 'digest ir)) 0 #\x))
          (act (string-set! (cdr (assq 'scannerDigest receipt)) 0 #\x))
          (equal
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner ir "if") #f))
            "contextual scanner requires compiled IR and source")
          (sequential ((b (prepare-contextual-scanner plan "if"))
                 (initial (contextual-scanner-initial-state b))
                 (canonical (contextual-scan-state-canonical initial)))
            (equal (cdr (assq 'scannerDigest canonical))  digest)
            (scope-values (((token next) (contextual-scanner-step b initial 'token)))
              (equal (token-kind token)  'keyword)
              (equal (token-lexeme token)  "if")))
          (scope ((b (prepare-contextual-scanner plan "αβ")))
            (scope-values (((token next) (contextual-scanner-step b (contextual-scanner-initial-state b) 'token)))
              (equal (token-kind token)  'word)
              (equal (token-end token)  4))))))
    (scenario "scanner plans reject cycles and callback data"
      (scope ((cycle (list 'cycle)))
        (act (set-cdr! cycle cycle))
        (equal (with-catch (lambda (condition) (error-message condition))
                 (lambda () (prepare-contextual-scanner-plan cycle) #f))
                "cyclic contextual scanner plan IR"))
      (equal (with-catch (lambda (condition) (error-message condition))
               (lambda () (prepare-contextual-scanner-plan (list (lambda () #f))) #f))
              "contextual scanner plan requires closed IR data"))
    (scenario "dispatch indexing preserves negative sparse and duplicate cells"
      (scope-values (((_scanner ir) (bash-scanner ";")))
        (sequential ((cells (cdr (assq 'cells ir)))
               (target (find (lambda (cell) (equal? (take cell 3) '(command argument operator))) cells))
               (negative (map (lambda (cell) (if (eq? cell target)
                                                (append (take cell 3) '(#f)) cell)) cells))
               (sparse (filter (lambda (cell) (not (eq? cell target))) cells)))
          (each (rows) ((list negative sparse))
             (scope ((scanner (prepare-contextual-scanner (with-cells ir rows) ";")))
               (equal
                (with-catch (lambda (condition) (error-message condition))
                  (lambda () (contextual-scanner-step scanner
                               (contextual-scanner-initial-state scanner) 'argument) #f))
                 "missing contextual scanner dispatch")))
          (equal
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner (with-cells ir (cons target negative)) ";") #f))
            "duplicate contextual scanner dispatch cell")
          (sequential ((wide (append cells
                        (map (lambda (n) (list 'command 'argument
                                          (string->symbol (string-append "extra" (number->string n)))
                                          '(unused))) (iota 9))))
                 (single (filter (lambda (cell) (eq? (cadr cell) 'argument)) cells))
                 (structural (cons (list '(foreign mode) 'argument 'foreign '(unused)) cells)))
            (each (rows) ((list wide single structural))
               (scope ((scanner (prepare-contextual-scanner (with-cells ir rows) ";")))
                 (scope-values (((token next) (contextual-scanner-step scanner
                                              (contextual-scanner-initial-state scanner) 'argument)))
                   (equal (token-kind token)  'operator)
                   (equal (token-lexeme token)  ";"))))))))
    (scenario "position admission rejects undeclared identities even at EOF"
      (scope-values (((scanner _ir) (bash-scanner "if")))
        (scope ((initial (contextual-scanner-initial-state scanner)))
          (equal
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (contextual-scanner-step scanner initial (gensym 'argument)) #f))
            "invalid contextual scanner checkpoint or position")
          (scope-values (((token next) (contextual-scanner-step scanner initial 'argument)))
            (equal (token-kind token)  'word)
            (equal
             (with-catch (lambda (condition) (error-message condition))
               (lambda () (contextual-scanner-step scanner next 'undeclared) #f))
              "invalid contextual scanner checkpoint or position")
            (scope-values (((token final) (contextual-scanner-step scanner next 'command-start)))
              (equal token  #f)
              (equal (contextual-scan-state-byte-offset final)  2))))))
    (scenario "Bash nested word scans without a language callback"
      (scope-values (((scanner ir)
                    (bash-scanner "if printf %s \"${x:-$(printf '%s' '}')}\"\n")))
        (iterate loop ((state (contextual-scanner-initial-state scanner))
                   (position 'command-start)
                   (tokens '()))
          (scope-values (((token next)
                        (contextual-scanner-step scanner state position)))
            (branch token
              (continue loop next 'argument (cons token tokens))
              (scope ((found (reverse tokens)))
                (equal (map token-kind found)
                        '(reserved-word horizontal-whitespace word
                             horizontal-whitespace word
                             horizontal-whitespace word newline))
                (equal (token-lexeme (list-ref found 6))
                        "\"${x:-$(printf '%s' '}')}\"")
                (equal (contextual-scan-state-byte-offset next)
                        (u8vector-length
                           (string->utf8
                            "if printf %s \"${x:-$(printf '%s' '}')}\"\n")))
                (equal (string? (contextual-scanner-ir-ref ir 'digest))
                        #t)))))))
    (scenario "HCL template and expression modes share the executor"
      (scope-values (((scanner _ir) (hcl-template-scanner "hello ${name} α")))
        (sequential ((initial (contextual-scanner-initial-state scanner))
               (tokens '()))
(locals ((take (lambda ( state position)
            (let-values (((token next)
                          (contextual-scanner-step scanner state position)))
              (set! tokens (append tokens (list token)))
              next))))

          (sequential ((a (take initial 'text))
                 (b (take a 'text))
                 (c (take b 'text))
                 (d (take (contextual-scan-state-with-mode c 'expression)
                          'expression))
                 (e (take d 'expression))
                 (f (take (contextual-scan-state-with-mode e 'template) 'text))
                 (g (take f 'text)))
            (equal (map token-kind tokens)
                    '(template-text template-text interpolation-open
                         identifier interpolation-close template-text
                         template-text))
            (equal (contextual-scan-state-mode g)  'template)
            (equal (token-start (list-ref tokens 6))  (token-end (list-ref tokens 5)))
            (equal (contextual-scan-state-byte-offset g)
                    (u8vector-length (string->utf8 "hello ${name} α"))))))))
    (scenario "literal catalogs preserve longest complete Unicode prefixes across modes"
      (sequential ((role (make-contextual-role 'prefixes
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
        (scope-values (((a next) (contextual-scanner-step
                                scanner (contextual-scanner-initial-state scanner) 'token)))
          (equal (token-lexeme a)  "αβ")
          (equal (list (token-start a) (token-end a))  '(0 4))
          (scope-values (((b last) (contextual-scanner-step
                                  scanner (contextual-scan-state-with-mode next 'second) 'token)))
            (equal (token-lexeme b)  "αβ")
            (equal (list (token-start b) (token-end b))  '(4 8))
            (equal (contextual-scan-state-byte-offset last)  8)
            (equal
             (with-catch (lambda (condition) (error-message condition))
               (lambda () (contextual-scanner-step scanner last 'token) #f))
              "contextual scanner has no match")))))
    (scenario "closed guards retain Unicode exceptions and prefix rejection"
      (sequential ((role (make-contextual-role 'guarded
                     (list (method 'word 'any 'any 'word 'word)
                           (method 'literal 'any 'any 'literal 'literal))))
             (dispatch (compile-contextual-dispatch
                        (list role) '(normal) '(token) '(word literal)))
             (ir (compile-contextual-scanner
                  (list (rule 'guarded-word 'normal 'word
                              '(unless-prefix ("ab" "λ") ("abc" "λx") (identifier)) 10)
                        (rule 'fallback 'normal 'literal '(literals ("ab" "λ")) 0))
                  dispatch 'normal)))
        (each (input) ('(("abc" word "abc" 3) ("abx" literal "ab" 2)
           ("λx" word "λx" 3) ("λz" literal "λ" 2)
           ("z" word "z" 1)))
           (each (recipe) ((list ir (prepare-contextual-scanner-plan ir)))
              (scope ((scanner (prepare-contextual-scanner recipe (car input))))
                (scope-values (((token next) (contextual-scanner-step
                                           scanner (contextual-scanner-initial-state scanner) 'token)))
                  (equal (list (token-kind token) (token-lexeme token) (token-end token))
                          (cdr input))))))))
    (scenario "candidate selection preserves length before rank in either declaration order"
      (sequential ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long))))
        (each (rows) ('(((short normal short (literal "λ") 100) (long normal long (literal "λx") 0))
           ((long normal long (literal "λx") 0) (short normal short (literal "λ") 100))
           ((short normal short (literal "λx") 0) (long normal long (literal "λx") 1))
           ((long normal long (literal "λx") 1) (short normal short (literal "λx") 0))))
           (scope ((ir (compile-contextual-scanner (map (lambda (row) (apply rule row)) rows) dispatch 'normal)))
             (each (recipe) ((list ir (prepare-contextual-scanner-plan ir)))
                (scope ((scanner (prepare-contextual-scanner recipe "λx")))
                  (scope-values (((token next) (contextual-scanner-step scanner
                                             (contextual-scanner-initial-state scanner) 'token)))
                    (equal (list (token-kind token) (token-lexeme token) (token-end token)
                                 (contextual-scan-state-byte-offset next))  '(long "λx" 3 3)))))))))
    (scenario "candidate conflicts remain eager even before a longer later match"
      (sequential ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long)))
             (ir (compile-contextual-scanner
                  (list (rule 'first 'normal 'short '(literal "λ") 0)
                        (rule 'conflict 'normal 'long '(literals ("λ" "Ω")) 0)
                        (rule 'later 'normal 'long '(literal "λx") 0)) dispatch 'normal)))
        (each (recipe) ((list ir (prepare-contextual-scanner-plan ir)))
           (scope ((scanner (prepare-contextual-scanner recipe "λx")))
             (equal (with-catch error-message
                      (lambda () (contextual-scanner-step scanner
                                   (contextual-scanner-initial-state scanner) 'token) #f))
                     "ambiguous contextual scanner match")))))
    (scenario "equivalent ties preserve the first rule in conflict diagnostics"
      (sequential ((role (make-contextual-role 'selection
                     (list (method 'short 'any 'any 'short 'short)
                           (method 'long 'any 'any 'long 'long))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(short long)))
             (ir (compile-contextual-scanner
                  (list (rule 'first 'normal 'short '(literal "λ") 0)
                        (rule 'equivalent 'normal 'short '(literals ("λ" "Ω")) 0)
                        (rule 'conflict 'normal 'long '(unless-prefix ("Ω") () (literal "λ")) 0))
                  dispatch 'normal)))
        (each (recipe) ((list ir (prepare-contextual-scanner-plan ir)))
           (scope ((scanner (prepare-contextual-scanner recipe "λ")))
             (equal (with-catch error-irritants
                      (lambda () (contextual-scanner-step scanner
                                   (contextual-scanner-initial-state scanner) 'token) #f))
                     '(first conflict))))))
    (property "equal token forms do not hide conflicting scanner actions"
      (bindings (role (make-contextual-role
                    'redirect
                    (list (method 'redirect 'any 'any 'redirect 'redirect)))) (dispatch (compile-contextual-dispatch
                        (list role) '(command) '(command) '(redirect))) (ir (compile-contextual-scanner
                  (list (rule 'open 'command 'redirect '(literal "<<") 0
                              '(expect-marker #f))
                        (rule 'plain 'command 'redirect
                              '(literals ("<<")) 0 'keep))
                  dispatch 'command)) (scanner (prepare-contextual-scanner (prepare-contextual-scanner-plan ir) "<<")))
      (equal
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (contextual-scanner-step
             scanner (contextual-scanner-initial-state scanner) 'command)
            #f))
          "ambiguous contextual scanner match"))
    (scenario "input IR cannot admit the private prepared matchers under guards"
      (each (matcher) ('((literal-trie 42) (prepared-region 42)
         (unless-prefix ("#") () (literal-trie 42))
         (unless-prefix ("#") () (prepared-region 42))))
      (scope-values (((_scanner ir) (bash-scanner "if")))
        (sequential ((body
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
          (equal
           (with-catch (lambda (condition) (error-message condition))
             (lambda () (prepare-contextual-scanner altered "if") #f))
            "private contextual scanner matcher in input IR")))))
    (scenario "a self-consistent digest cannot admit a foreign opcode contract"
      (scope-values (((_scanner ir) (bash-scanner "if")))
        (sequential ((body
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
          (equal
           (with-catch
            (lambda (condition) (error-message condition))
            (lambda () (prepare-contextual-scanner altered "if") #f))
            "contextual scanner requires compiled IR and source"))))
    (property "compiler rejects altered dispatch and duplicate scan candidates"
      (bindings (role (make-contextual-role
                    'validation
                    (list (method 'first 'any 'any 'first 'first)
                          (method 'second 'any 'any 'second 'second)))) (dispatch (compile-contextual-dispatch
                        (list role) '(normal) '(start) '(first second))) (first (rule 'first 'normal 'first '(literal "x") 0)) (second (rule 'second 'normal 'second '(literal "x") 0)) (altered
              (map (lambda (row)
                     (if (eq? (car row) 'cells)
                       (cons 'cells '()) row))
                   dispatch)))
      (equal
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (compile-contextual-scanner (list first second)
                                        dispatch 'normal)
            #f))
          "duplicate contextual scan candidate")
      (equal
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (compile-contextual-scanner (list first) altered 'normal)
            #f))
          "invalid contextual scanner declaration")))

(deflanguage-parser-tests contextual-literal-width-test "prepared literal scalar byte widths"
  (engine)
    (scenario "ASCII and every Unicode width retain byte endpoints"
      (sequential ((role (make-contextual-role 'widths
                     (list (method 'literal 'any 'any 'literal 'literal))))
             (dispatch (compile-contextual-dispatch (list role) '(normal) '(token) '(literal)))
             (ir (compile-contextual-scanner
                  (map (lambda (text) (rule (string->symbol text) 'normal 'literal
                                            (list 'literal text) 0))
                       '("a" "α" "猫" "😀")) dispatch 'normal)))
        (each (input) ((list ir (prepare-contextual-scanner-plan ir)))
          (scope ((scanner (prepare-contextual-scanner input "aα猫😀")) (start 0))
            (iterate loop ((state (contextual-scanner-initial-state scanner))
                       (expected '(1 3 6 10)))
              (unless (null? expected)
                (scope-values (((token next) (contextual-scanner-step scanner state 'token)))
                  (equal (token-start token)  start)
                  (equal (token-end token)  (car expected))
                  (equal (contextual-scan-state-byte-offset next)  (car expected))
                  (act (set! start (car expected)))
                  (continue loop next (cdr expected))))))))))
