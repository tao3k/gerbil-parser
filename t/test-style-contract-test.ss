;;; -*- Gerbil -*-
;;; Source-level AST contract: tests assert values, never print snapshots.

(import (only-in :std/test check test-case test-suite))
(export test-style-contract-tests)

(def (test-form-allowed? form forbidden)
  (cond
   ((not (pair? form)) #t)
   ((and (symbol? (car form))
         (memq (car form) '(quote quasiquote syntax quasisyntax))) #t)
   ((and (symbol? (car form)) (memq (car form) forbidden)) #f)
   (else (and (test-form-allowed? (car form) forbidden)
              (test-form-allowed? (cdr form) forbidden)))))

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
                "t/generate-event-strategy-fixture.ss"
                "t/generate-event-strategy-grammar.ss"
                "t/generate-owned-word-fixture.ss"))
             => '()))))
