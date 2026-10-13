;;; -*- Gerbil -*-
;;; Source-form correspondence for the semantic-preserving runtime split.
(import (only-in :std/test check test-case test-suite)
        (only-in :std/misc/process run-process))
(export runtime-module-organization-test)
(def base "f57d2aa6502bd39b62119cdd2e5a95469ef6fd9f")
(def (read-forms port)
  (let loop ((forms '()))
    (let (form (read port))
      (if (eof-object? form) (reverse forms) (loop (cons form forms))))))
(def (behavior forms)
  (filter (lambda (form) (not (memq (car form) '(import export)))) forms))
(def (same-forms? expected actual)
  (let loop ((expected expected) (actual actual))
    (if (null? expected) (null? actual)
      (let remove ((before '()) (rest actual))
        (cond
         ((null? rest) #f)
         ((equal? (car expected) (car rest))
          (loop (cdr expected) (append (reverse before) (cdr rest))))
         (else (remove (cons (car rest) before) (cdr rest))))))))
(def (check-family family)
  (let* ((facade (string-append "src/runtime/" family ".ss"))
         (directory (string-append "src/runtime/" family))
         (original (run-process (list "git" "show" (string-append base ":" facade))
                                coprocess: read-forms))
         (current (call-with-input-file facade read-forms))
         (owners (map (lambda (name)
                        (call-with-input-file (path-expand name directory) read-forms))
                      (filter (lambda (name) (equal? (path-extension name) ".ss"))
                              (directory-files directory)))))
    (check (behavior current) => '())
    (check (filter (lambda (f) (eq? (car f) 'export)) current)
           => (filter (lambda (f) (eq? (car f) 'export)) original))
    (check (same-forms? (behavior original) (apply append (map behavior owners))) => #t)))
(def runtime-module-organization-test
  (test-suite "Runtime module organization correspondence"
    (test-case "LR facade and private owners retain every execution form exactly once"
      (check-family "lr-parser"))
    (test-case "incremental facade and owners retain every execution form exactly once"
      (check-family "incremental"))
    (test-case "scan facade and pure lexical owners retain every execution form exactly once"
      (check-family "scan"))))
