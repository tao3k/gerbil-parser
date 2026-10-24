;;; SDK binding/visitor controls for the shared source execution capability.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source
                 source-expression-procedure source-expression-dependencies source-procedure
                 read-source-definition-port read-source-subdefinition source-mutation-sites transform-source-declaration)
        (only-in :gerbil/expander current-expander-context make-top-context
                 eval-syntax core-bind-syntax! datum->syntax stx-car))

(def (single-expansion-observation)
  (parameterize ((current-expander-context (make-top-context)))
    (eval-syntax '(import :gerbil/core))
    (let (expansions 0)
      (core-bind-syntax! (datum->syntax #f 'source-dependency-once)
        (lambda (stx)
          (set! expansions (+ expansions 1))
          (when (> expansions 1) (error "source was expanded twice"))
          (datum->syntax (stx-car stx) '(lambda () 42))))
      (let (procedure (source-expression-procedure '(source-dependency-once)))
        (list expansions (procedure))))))

(def (with-ambient-host-value procedure)
  (let* ((cell (##make-global-var '##unbound-source-ambient)) (previous (##global-var-ref cell)))
    (try
      (##global-var-set! cell (lambda () 42))
      (procedure)
      (finally (##global-var-set! cell previous)))))

(deflanguage-parser-tests source-dependency-test "complete source dependency admission"
  (engine (exclusive #t))
  (scenario "declaration reading retains complete forms and rejects ambiguity"
    (equal (call-with-input-string "'(def (wanted) ignored) (def (other) 0) (def (wanted x) x 'tail)"
             (lambda (port) (read-source-definition-port port 'wanted)))
           '(def (wanted x) x 'tail))
    (raises (call-with-input-string "(def (wanted) 1) (def (wanted) 2)"
              (lambda (port) (read-source-definition-port port 'wanted))) true)
    (raises (call-with-input-string "(def (other) 1)"
              (lambda (port) (read-source-definition-port port 'wanted))) true)
    (raises (call-with-input-string "(def (wanted) 1) ("
              (lambda (port) (read-source-definition-port port 'wanted))) true))
  (scenario "syntax data cannot become nested source declarations"
    (each (owner) ('(quoted-source-owner syntax-source-owner syntax-alias-source-owner))
      (equal (with-catch error-message
               (lambda () (read-source-subdefinition "t/fixtures/quoted-source-declarations.ss" owner 'wanted)))
             "expected one nested source declaration"))
    (equal (read-source-subdefinition "t/fixtures/quoted-source-declarations.ss" 'mixed-source-owner 'wanted)
           '(def (wanted) 'actual))
    (raises (read-source-subdefinition "t/fixtures/quoted-source-declarations.ss" 'duplicate-source-owner 'wanted) true))
  (scenario "mutation witnesses use the same syntax-data barriers and source order"
    (equal (source-mutation-sites '()) '())
    (equal (source-mutation-sites
             '(begin (quote (set! quoted 0)) (quote-syntax (vector-set! quoted 0 0))
                     (syntax (set! quoted 0)) (set! live (vector-set! live 0 1))))
           '((set! live (vector-set! live 0 1)) (vector-set! live 0 1))))
  (scenario "transformations execute once and cannot replace the lexical interface"
    (sequential ((calls 0)
                 (result (transform-source-declaration '(def (wanted x) x)
                           (lambda (declaration) (set! calls (+ calls 1))
                             '(def (wanted x) x 'tail)))))
      (equal calls 1)
      (equal result '(def (wanted x) x 'tail)))
    (raises (transform-source-declaration '(def wanted 1) identity) true)
    (raises (transform-source-declaration '(def (wanted x) x)
              (lambda (_) '(def (other x) x))) true)
    (raises (transform-source-declaration '(def (wanted x) x)
              (lambda (_) '(def (wanted x unbound-source-tail) x))) true))
  (scenario "in-place header mutations cannot change the retained source or snapshot"
    (sequential ((declaration (list 'def (list 'wanted 'x) 'x)))
      (raises (transform-source-declaration declaration
                (lambda (copy) (set-cdr! (cadr copy) '(x unbound-source-tail)) copy)) true)
      (equal declaration '(def (wanted x) x)))
    (sequential ((default (string-copy "original"))
                 (declaration (list 'def (list 'wanted (list 'x default)) 'x)))
      (raises (transform-source-declaration declaration
                (lambda (copy) (string-set! (cadadr (cadr copy)) 0 #\X) copy)) true)
      (equal default "original")))
  (scenario "published declarations do not alias retained callback output"
    (sequential ((held #f)
                 (published (transform-source-declaration '(def (wanted x) x)
                              (lambda (copy) (set! held copy) copy))))
      (act (set-car! (cadr held) 'retargeted))
      (act (set-car! (cddr held) '(unbound-source-tail)))
      (equal published '(def (wanted x) x)))
    (sequential ((shared (list 'def '(wanted) (vector (string-copy "stable"))))
                 (published (transform-source-declaration '(def (wanted) 0) (lambda (_) shared))))
      (act (string-set! (vector-ref (caddr shared) 0) 0 #\X))
      (act (vector-set! (caddr shared) 0 "replaced"))
      (equal published '(def (wanted) #("stable")))))
  (scenario "complete source requests retain isolated literal storage"
    (sequential ((held #f)
                 (procedure (source-procedure "src/compiler/lr-compiler.ss" 'dynamic-precedence? '()
                              (lambda (declaration)
                                (set! held (append declaration (list (string-copy "stable")))) held))))
      (act (string-set! (last held) 0 #\X))
      (equal (procedure #f) "stable")))
  (scenario "helper storage slots do not supply source-level lexical references"
    (each (name) ('(declaration transformed header output))
      (raises (source-procedure "src/compiler/lr-compiler.ss" 'dynamic-precedence? '()
                (lambda (declaration) (append declaration (list name)))) true))
    (sequential ((procedure (source-procedure "src/compiler/lr-compiler.ss" 'dynamic-precedence? '(declaration)
                              (lambda (declaration) (append declaration '(declaration))))))
      (equal (procedure #f "explicit source capability") "explicit source capability")))
  (scenario "new parameters cannot hide dependencies introduced by a transform"
    (raises (source-procedure "src/compiler/lr-compiler.ss" 'dynamic-precedence? '()
              (lambda (declaration)
                (cons 'def
                  (cons (append (cadr declaration) '(unbound-source-tail))
                        (append (cddr declaration) '((unbound-source-tail))))))) true))
  (scenario "transformed trailing dependencies cannot bypass admission"
    (raises (source-procedure "src/compiler/lr-compiler.ss" 'dynamic-precedence? '()
              (lambda (declaration) (append declaration '((unbound-source-tail))))) true))
  (scenario "read targets and nested closure bodies are checked before execution"
    (equal (source-expression-dependencies '(lambda (x) (unbound-source-callback x)))
           '(unbound-source-callback))
    (equal (source-expression-dependencies '(lambda () (lambda () (unbound-source-closure))))
           '(unbound-source-closure))
    (raises (source-expression-procedure '(lambda () (unbound-source-callback))) true))
  (scenario "host primitives are resolved by the actual primitive slot"
    (equal (source-expression-dependencies '(lambda (x) (##car x) (##current-time-point))) '())
    (equal (source-expression-dependencies '(lambda () (##unbound-source-host)))
           '(##unbound-source-host)))
  (scenario "an ambient ## global function is not a primitive capability"
    (using (with-ambient-host-value) ()
      (equal (source-expression-dependencies '(lambda () (##unbound-source-ambient)))
             '(##unbound-source-ambient))
      (raises (source-expression-procedure '(lambda () (##unbound-source-ambient))) true)))
  (scenario "assignment includes its target and complete RHS in compiler traversal order"
    (equal (source-expression-dependencies '(lambda () (set! unbound-source-target (unbound-source-rhs))))
           '(unbound-source-target unbound-source-rhs))
    (equal (source-expression-dependencies '(lambda (x) (set! x (unbound-source-rhs))))
           '(unbound-source-rhs)))
  (scenario "local shadowing mutation and lexical capture are not missing imports"
    (equal (source-expression-dependencies
             '(lambda (x) (let ((unbound-source-callback x))
                            (lambda () (set! x unbound-source-callback) x)))) '())
    (sequential ((factory (source-expression-procedure
                           '(lambda (x) (lambda () (set! x (+ x 1)) x))))
                 (counter (factory 0)))
      (equal (counter) 1)
      (equal (counter) 2)))
  (scenario "optional defaults remain dependencies even before their branch executes"
    (equal (source-expression-dependencies '(lambda ((x (unbound-source-default))) x))
           '(unbound-source-default)))
  (scenario "quote and quote-syntax data do not become executable dependencies"
    (equal (source-expression-dependencies '(lambda () '(unbound-source-data))) '())
    (equal (source-expression-dependencies '(lambda () (quote-syntax unbound-source-syntax))) '()))
  (scenario "actual LR producer rejects the former incomplete imported environment"
    (raises (source-procedure "src/compiler/lr-table.ss" 'production-vocabulary
              '(production-lhs production-terminal-catalog)) true)
    (equal (procedure? (source-procedure "src/compiler/lr-table.ss" 'production-vocabulary
                        '(production-lhs production-rhs base-symbol nonterminal-symbol?
                          nonterminal-name production-terminal-catalog))) #t))
  (scenario "all trailing body forms participate in dependency admission"
    (equal (source-expression-dependencies
             '(lambda () 'first (unbound-source-tail))) '(unbound-source-tail))
    (raises (source-expression-procedure '(lambda () 'first (unbound-source-tail))) true))
  (scenario "a checked macro expands exactly once and the retained syntax executes"
    (equal (single-expansion-observation) '(1 42)))
  (scenario "closed data and computed procedures do not bypass the literal guard"
    (each (expression) ('(42 'closed (if #t (lambda () 42) (lambda () 0))))
      (equal (source-expression-dependencies expression) '())
      (raises (source-expression-procedure expression) true))
    (sequential ((procedure (source-expression-procedure
                             '(case-lambda (() 42) ((x) x)))))
      (equal (procedure) 42)
      (equal (procedure 7) 7)))
  (scenario "nonliteral setup cannot execute while preparing a source procedure"
    (equal (with-catch error-message
             (lambda () (source-expression-procedure
                          '(begin (error "must not execute") (lambda () 42)))))
           "source witness must expand to a procedure literal")))
