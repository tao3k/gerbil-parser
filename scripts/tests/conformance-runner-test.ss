#!/usr/bin/env gxi
(import :std/test
        (only-in :std/test/base TestConfig current-test-config VERBOSITY-CASE
                 test-suite! test-result-ok?))
;; Exercise the production functions themselves without expanding language packs.
(call-with-input-file "t/conformance-main.ss"
  (lambda (port)
    (let loop ()
      (let (form (read port))
        (unless (eof-object? form)
          (when (and (pair? form) (eq? (car form) 'def)
                     (pair? (cadr form))
                     (memq (caadr form) '(validate-conformance-suites! run-suite-row!)))
            (eval form))
          (loop))))))
(call-with-input-file "build-language-conformance-link.ss"
  (lambda (port)
    (let loop ()
      (let (form (read port))
        (unless (eof-object? form)
          (when (and (pair? form) (eq? (car form) 'def) (pair? (cadr form))
                     (memq (caadr form) '(test-object-source? test-initialization-options static-translation-command)))
            (eval form))
          (loop))))))

;; Same compiler-side expression used by the link owner; the command protocol
;; test below verifies generated-code options precede this evaluation boundary.
(def compiler-progress-expression
  "(begin (port-settings-set! (current-output-port) (list buffering: #f)) (gc-report-set! #t))")
(def (main mode . _)
  (let* ((owner (current-thread))
         (good (test-suite "control" (test-case "caller ownership" (check (current-thread) => owner)))))
    (cond
      ((equal? mode "initialization")
       (for-each (lambda (source)
         (let (options (test-initialization-options source))
           (check (list (car options) (caddr options)) => '("-prelude" "-postlude"))
           (check (static-translation-command "gsc" "output.c" source options)
             => (append (list "gsc" "-verbose" "-c" "-o" "output.c") options
                        (list "-e" compiler-progress-expression source)))
           (check (call-with-output-string
                    (lambda (port)
                      (parameterize ((current-output-port port))
                        (eval (read (open-input-string (cadr options))))
                        (eval (read (open-input-string (cadddr options)))))))
             => (string-append "CONFORMANCE-MODULE-START " (path-strip-directory source) "\n"
                               "CONFORMANCE-MODULE-READY " (path-strip-directory source) "\n"))))
         '("static/gerbil-parser__t__example-test.scm" "static/gerbil-parser__languages__bash__parser-test.scm"))
       (check (test-initialization-options "static/gerbil-parser__src__compiler__lr.scm") => '())
       (check (test-initialization-options "static/gerbil-parser__t__fixtures__lr1-construction.scm") => '())
       (displayln "CONFORMANCE-CONTROL-OK initialization"))
      ((equal? mode "success")
       (validate-conformance-suites! (list (list "left" good)))
       (run-suite-row! (list "left" good))
       (run-suite-row! (list "right" (test-suite "second" (test-case "same caller" (check (current-thread) => owner)))))
       (displayln "CONFORMANCE-CONTROL-OK success"))
      ((equal? mode "duplicate")
       (validate-conformance-suites! (list (list "left" good) (list "right" good)))
       (error "duplicate Suite was accepted"))
      ((equal? mode "failure")
       (run-suite-row! (list "failure" (test-suite "failure" (test-case "negative control" (check #f => #t)))))
       (error "failed Suite was accepted"))
      ((equal? mode "timeout")
       (run-suite-row! (list "timeout" (test-suite "timeout" (test-case "blocked" (thread-sleep! 2)))) 0.1)
       (error "timed-out Suite was accepted"))
      (else (error "unknown control mode" mode)))))
