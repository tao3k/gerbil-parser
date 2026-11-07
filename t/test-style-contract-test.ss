(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/testing/contracts parser-test-form-admitted?))
;;; -*- Gerbil -*-
;;; Source-level AST contract: tests assert values, never print snapshots.

(import (only-in :std/test check test-case test-suite))


(def (test-source-allowed? path forbidden (owned-output #f))
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (or (eof-object? form)
              (and (if owned-output
                     (parser-test-form-admitted? form forbidden owned-output)
                     (parser-test-form-admitted? form forbidden)) (loop))))))))

(def (test-sources root)
  (apply append
         (map (lambda (name)
                (let* ((path (path-expand name root))
                       (info (file-info path)))
                  (cond
                   ((eq? (file-info-type info) 'directory)
                    (test-sources path))
                   ((or (string-suffix? "-test.ss" name)
                        (string-suffix? ".ssi" name)) (list path))
                   (else '()))))
              (directory-files root))))

(deflanguage-parser-tests test-style-contract-tests "parser test AST contract"
  (engine)
    (property "reject output calls but allow quoted negative fixtures"
      (bindings)
      (equal (parser-test-form-admitted? '(display "snapshot") '(display))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-string (lambda (port) (display "input" port)))
              '(display))  #t)
      (equal (parser-test-form-admitted?
              '(call-with-output-string (lambda (port) (display "Rust" port)))
              '(display displayln write-string string-append format))  #f)
      (equal (parser-test-form-admitted? '(display "snapshot" port) '(display))  #f)
      (equal (parser-test-form-admitted? '(display "snapshot" (current-output-port))
                                 '(display))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-string
                 (lambda (port) (display (display "snapshot") port)))
              '(display))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-string
                 (lambda (port) (lambda (port) (display "snapshot" port))))
              '(display))  #f)
      (equal (parser-test-form-admitted? '(quote (display input)) '(display))  #t)
      (equal (parser-test-form-admitted? '(check value => expected) '(display))
              #t))
    (property "owned fixture ports retain the console and assembly restrictions"
      (bindings)
      (equal (parser-test-form-admitted?
              '(call-with-output-string (lambda (port) (display "input" port)))
              '(display displayln))  #t)
      (equal (parser-test-form-admitted? '(display "snapshot" (current-output-port))
                                '(display))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-file path (lambda (port) (write-string ir port)))
              '(display write-string string-append))  #t)
      (equal (parser-test-form-admitted?
              '(call-with-output-file path
                 (lambda (port) (write-string (string-append "Rust" "text") port)))
              '(write-string string-append))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-file quote (display "snapshot") (lambda (port) #t))
              '(display))  #f)
      (equal (parser-test-form-admitted?
              '(call-with-output-string (lambda (port) quote (display "snapshot")))
              '(display))  #f))
    (scenario "deep and wide ASTs retain rejection and lexical port authority"
      (iterate deep ((n 20000) (form '(display "snapshot")))
        (branch (zero? n)
          (equal (parser-test-form-admitted? form '(display))  #f)
          (continue deep (- n 1) (list 'begin form))))
      (equal (parser-test-form-admitted? (cons 'begin (make-list 20000 '(check value => expected)))
                                '(display))  #t)
      (iterate deep ((n 20000) (form '(display "fixture" port)))
        (branch (zero? n)
          (group
            (equal (parser-test-form-admitted? (list 'call-with-output-string (list 'lambda '(port) form))
                                      '(display))  #t)
            (equal (parser-test-form-admitted? (list 'call-with-output-string
                                           (list 'lambda '(port) (list 'lambda '(port) form)))
                                      '(display))  #f))
          (continue deep (- n 1) (list 'begin form)))))
    (property "all parser tests avoid display assertions"
      (bindings)
      (equal (filter (lambda (path)
                       (not (test-source-allowed? path
                                                  '(display displayln))))
                     (append (test-sources "t")
                             (test-sources "languages")))
              '()))
    (property "source witnesses declare data and keep proofs in Lean"
      (bindings)
      (equal (filter
              (lambda (path)
                (not (test-source-allowed?
                       path '(display displayln write-string string-append format))))
              '("t/fixtures/event-walk-source.ss"
                "t/generate-walk-source-witness.ss"
                "t/event-walk-source-test.ss"))
              '()))
    (property "parser tests declare suites and shared checks through the engine"
      (bindings)
      (equal (filter
               (lambda (path)
                 (not (test-source-allowed? path
                       '(check check-exception checkf check-eq? check-eqv? check-equal?
                         test-suite test-case test-case-add!
                         test-worker-exclusive? test-worker-affinity))))
               (append (test-sources "t") (test-sources "languages")
                       '("t/line-structure-assertions.ss" "t/rust-aot-test-syntax.ss")))
             '()))
    (property "declaration metadata and arguments are not application tails"
      (bindings)
      (equal (parser-test-form-admitted?
               '(import (only-in :std/test check test-case test-suite))
               '(check test-case test-suite)) #t)
      (equal (parser-test-form-admitted?
               '(scope ((check value)) (verify check)) '(check test-case)) #t)
      (equal (parser-test-form-admitted?
               '(lambda ((value (display "snapshot"))) value) '(display displayln)) #f))
    (scenario "borrowed fixture ports reject reassignment and lexical shadows"
      (each (body) ('((set! port (current-output-port))
                     (lambda port (display "snapshot" port))
                     (lambda (x . port) (display "snapshot" port))
                     (let (port (current-output-port)) (display "snapshot" port))
                     (scope ((port (current-output-port))) (act (display "snapshot" port)))))
        (equal (parser-test-form-admitted?
                 (list 'call-with-output-string (list 'lambda '(port) body))
                 '(display displayln)) #f)))
    (property "template families cannot hide an executable unquote"
      (bindings)
      (equal (parser-test-form-admitted?
               '(quasiquote (quasisyntax (unquote (display "snapshot"))))
               '(display displayln)) #f)
      (equal (parser-test-form-admitted?
               '(quasisyntax (quasiquote (unsyntax (display "snapshot"))))
               '(display displayln)) #f))
    (property "grammar witness producers publish only through the shared data macro"
      (bindings)
      (equal (parser-test-form-admitted?
               '(call-with-output-string (lambda (port) (write-string "def fixture := []" port)))
               '(display displayln write write-string write-char newline force-output format) '()) #f)
      (equal (test-source-allowed? "t/fixtures/lr1-construction.ss"
               '(display displayln write write-string write-char newline force-output format) '()) #t)
      (equal (test-source-allowed? "t/generate-grammar-model-facts.ss"
               '(display displayln write write-string write-char newline force-output format) '()) #t))
    (property "Rust AOT tests avoid textual source assembly"
      (bindings)
      (equal (test-source-allowed? "t/rust-syntax-test.ss"
                                   '(display displayln string-append))  #t)
      (equal (test-source-allowed? "t/rust-aot-test-syntax.ss"
                                   '(display displayln string-append))  #t))
    (property "Scheme event strategies and generators cannot assemble Rust text"
      (bindings)
      (equal (filter
              (lambda (path)
                (not (test-source-allowed?
                      path '(display displayln write-string string-append format))))
              '("src/compiler/event-strategy-aot.ss"
                "src/compiler/event-fold-aot.ss"
                "t/generate-event-strategy-fixture.ss"
                "t/generate-event-strategy-grammar.ss"
                "t/generate-owned-word-fixture.ss"))
              '()))
  (property "raw owner bindings reject while declaration metadata and quoted controls remain valid"
    (bindings)
    (equal (parser-test-form-admitted? '(def test-worker-exclusive? #t)
             '(test-worker-exclusive? test-worker-affinity)) #f)
    (equal (parser-test-form-admitted? '(define test-worker-affinity 'runtime)
             '(test-worker-exclusive? test-worker-affinity)) #f)
    (equal (parser-test-form-admitted?
             '(deflanguage-parser-tests owner "owner" (engine)
                (property "value" (bindings) (equal 1 1)))
             '(test-worker-exclusive? test-worker-affinity)) #t)
    (equal (parser-test-form-admitted? '(quote (def test-worker-exclusive? #t))
             '(test-worker-exclusive? test-worker-affinity)) #t))
)

;; gxtest discovers only exported names ending in -test.
(def test-style-contract-test test-style-contract-tests)
(export test-style-contract-test)
