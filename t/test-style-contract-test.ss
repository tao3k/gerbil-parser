;;; -*- Gerbil -*-
;;; Source-level AST contract: tests assert values, never print snapshots.

(import (only-in :std/test check test-case test-suite))
(export test-style-contract-tests)

(def (test-form-allowed? form forbidden (owned-ports '()))
  (cond
   ((not (pair? form)) #t)
   ((and (symbol? (car form))
         (memq (car form) '(quote quasiquote syntax quasisyntax))) #t)
   ((and (list? form)
         (memq (car form) '(call-with-output-string call-with-output-file))
         (pair? (cdr form)))
    (let (handler (last form))
      (if (and (list? handler) (>= (length handler) 3)
               (eq? (car handler) 'lambda)
               (list? (cadr handler)) (= (length (cadr handler)) 1)
               (symbol? (caadr handler)))
        (and
         (andmap (lambda (arg) (test-form-allowed? arg forbidden owned-ports))
                 (reverse (cdr (reverse (cdr form)))))
         (andmap (lambda (body)
                   (test-form-allowed? body forbidden
                                       (cons (caadr handler) owned-ports)))
                 (cddr handler)))
        (and (test-form-allowed? (car form) forbidden owned-ports)
             (test-form-allowed? (cdr form) forbidden owned-ports)))))
   ((and (symbol? (car form)) (memq (car form) forbidden))
    ;; Fixture construction and IR serialization may write to a fresh owned
    ;; port. Console display remains forbidden; payload assembly is inspected.
    (and (memq (car form) '(display write-string))
         (list? form) (= (length form) 3)
         (memq (caddr form) owned-ports)
         (test-form-allowed? (cadr form) forbidden owned-ports)))
   (else (and (test-form-allowed? (car form) forbidden owned-ports)
              (test-form-allowed? (cdr form) forbidden owned-ports)))))

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
              '(write-string string-append)) => #f))
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
                "t/generate-event-fold-ir.ss"
                "t/generate-owned-word-fixture.ss"))
             => '()))))

;; gxtest discovers only exported names ending in -test.
(def test-style-contract-test test-style-contract-tests)
(export test-style-contract-test)
