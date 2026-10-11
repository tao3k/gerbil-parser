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
(def (main mode . _)
  (let* ((owner (current-thread))
         (good (test-suite "control" (test-case "caller ownership" (check (current-thread) => owner)))))
    (cond
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
