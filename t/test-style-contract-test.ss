;;; -*- Gerbil -*-
;;; Source-level AST contract: tests assert values, never print snapshots.

(import (only-in :std/test check test-case test-suite))
(export test-style-contract-tests)

(def (test-form-allowed? form forbidden (owned-ports '()))
  ;; Explicit frames carry lexical port authority. Source width/depth must not
  ;; become native call-stack depth in the compiled contract checker.
  (let loop ((pending (list (cons form owned-ports))))
    (if (null? pending) #t
      (let* ((frame (car pending)) (form (car frame))
             (owned-ports (cdr frame)) (rest (cdr pending)))
        (def (push-forms forms ports next)
          (let push ((forms forms) (next next))
            (if (null? forms) next
              (push (cdr forms) (cons (cons (car forms) ports) next)))))
        (def (push-pair)
          (loop (cons (cons (car form) owned-ports)
                      (cons (cons (cdr form) owned-ports) rest))))
        (cond
         ((not (pair? form)) (loop rest))
         ((and (symbol? (car form))
               (memq (car form) '(quote quasiquote syntax quasisyntax)))
          (loop rest))
         ((and (list? form)
               (memq (car form) '(call-with-output-string call-with-output-file))
               (pair? (cdr form)))
          (let (handler (last form))
            (if (and (list? handler) (>= (length handler) 3)
                     (eq? (car handler) 'lambda)
                     (list? (cadr handler)) (= (length (cadr handler)) 1)
                     (symbol? (caadr handler)))
              (let ((args (reverse (cdr (reverse (cdr form)))))
                    (body-ports (cons (caadr handler) owned-ports)))
                (loop (push-forms args owned-ports
                                  (push-forms (cddr handler) body-ports rest))))
              (push-pair))))
         ((and (symbol? (car form)) (memq (car form) forbidden))
          ;; Fixture serialization owns its port; console output stays forbidden.
          (and (or (eq? (car form) 'write-string)
                   (and (eq? (car form) 'display)
                        (or (equal? forbidden '(display))
                            (equal? forbidden '(display displayln)))))
               (list? form) (= (length form) 3)
               (memq (caddr form) owned-ports)
               (loop (cons (cons (cadr form) owned-ports) rest))))
         ;; A nested lambda cannot inherit authority for a shadowed argument.
         ((and (eq? (car form) 'lambda) (pair? (cdr form))
               (list? (cadr form)))
          (loop (cons (cons (cddr form)
                            (filter (lambda (port) (not (memq port (cadr form))))
                                    owned-ports)) rest)))
         (else (push-pair)))))))

(def (test-source-allowed? path forbidden)
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (or (eof-object? form)
              (and (test-form-allowed? form forbidden) (loop))))))))

(def (test-sources root)
  (apply append
         (map (lambda (name)
                (let* ((path (path-expand name root))
                       (info (file-info path)))
                  (cond
                   ((eq? (file-info-type info) 'directory)
                    (test-sources path))
                   ((string-suffix? "-test.ss" name) (list path))
                   (else '()))))
              (directory-files root))))

(def test-style-contract-tests
  (test-suite "parser test AST contract"
    (test-case "reject output calls but allow quoted negative fixtures"
      (check (test-form-allowed? '(display "snapshot") '(display)) => #f)
      (check (test-form-allowed?
              '(call-with-output-string (lambda (port) (display "input" port)))
              '(display)) => #t)
      (check (test-form-allowed?
              '(call-with-output-string (lambda (port) (display "Rust" port)))
              '(display displayln write-string string-append format)) => #f)
      (check (test-form-allowed? '(display "snapshot" port) '(display)) => #f)
      (check (test-form-allowed? '(display "snapshot" (current-output-port))
                                 '(display)) => #f)
      (check (test-form-allowed?
              '(call-with-output-string
                 (lambda (port) (display (display "snapshot") port)))
              '(display)) => #f)
      (check (test-form-allowed?
              '(call-with-output-string
                 (lambda (port) (lambda (port) (display "snapshot" port))))
              '(display)) => #f)
      (check (test-form-allowed? '(quote (display input)) '(display)) => #t)
      (check (test-form-allowed? '(check value => expected) '(display))
             => #t))
    (test-case "owned fixture ports retain the console and assembly restrictions"
      (check (test-form-allowed?
              '(call-with-output-string (lambda (port) (display "input" port)))
              '(display displayln)) => #t)
      (check (test-form-allowed? '(display "snapshot" (current-output-port))
                                '(display)) => #f)
      (check (test-form-allowed?
              '(call-with-output-file path (lambda (port) (write-string ir port)))
              '(display write-string string-append)) => #t)
      (check (test-form-allowed?
              '(call-with-output-file path
                 (lambda (port) (write-string (string-append "Rust" "text") port)))
              '(write-string string-append)) => #f)
      (check (test-form-allowed?
              '(call-with-output-file quote (display "snapshot") (lambda (port) #t))
              '(display)) => #f)
      (check (test-form-allowed?
              '(call-with-output-string (lambda (port) quote (display "snapshot")))
              '(display)) => #f))
    (test-case "deep and wide ASTs retain rejection and lexical port authority"
      (let deep ((n 20000) (form '(display "snapshot")))
        (if (zero? n)
          (check (test-form-allowed? form '(display)) => #f)
          (deep (- n 1) (list 'begin form))))
      (check (test-form-allowed? (cons 'begin (make-list 20000 '(check value => expected)))
                                '(display)) => #t)
      (let deep ((n 20000) (form '(display "fixture" port)))
        (if (zero? n)
          (begin
            (check (test-form-allowed? (list 'call-with-output-string (list 'lambda '(port) form))
                                      '(display)) => #t)
            (check (test-form-allowed? (list 'call-with-output-string
                                           (list 'lambda '(port) (list 'lambda '(port) form)))
                                      '(display)) => #f))
          (deep (- n 1) (list 'begin form)))))
    (test-case "all parser tests avoid display assertions"
      (check (filter (lambda (path)
                       (not (test-source-allowed? path
                                                  '(display displayln))))
                     (append (test-sources "t")
                             (test-sources "languages")))
             => '()))
    (test-case "Rust AOT tests avoid textual source assembly"
      (check (test-source-allowed? "t/rust-syntax-test.ss"
                                   '(display displayln string-append)) => #t)
      (check (test-source-allowed? "t/rust-aot-test-syntax.ss"
                                   '(display displayln string-append)) => #t))
    (test-case "Scheme event strategies and generators cannot assemble Rust text"
      (check (filter
              (lambda (path)
                (not (test-source-allowed?
                      path '(display displayln write-string string-append format))))
              '("src/compiler/event-strategy-aot.ss"
                "src/compiler/event-fold-aot.ss"
                "t/generate-event-strategy-fixture.ss"
                "t/generate-event-strategy-grammar.ss"
                "t/generate-owned-word-fixture.ss"))
             => '()))))

;; gxtest discovers only exported names ending in -test.
(def test-style-contract-test test-style-contract-tests)
(export test-style-contract-test)
