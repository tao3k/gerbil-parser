#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "context-language" "tier-language"
        (rename-in "context-vocabulary" (deflanguage/tiers define-contextual-language))
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in "../../../src/runtime/parser" parse-source)
        (only-in "../../../src/runtime/artifact"
                 parse-artifact-success? parse-artifact-roundtrip parse-artifact-events))

(defsyntax (invalid-context-rows stx)
  (def (rejects form)
    (with-catch (lambda (condition) (error-message condition)) (lambda () (core-expand form) #f)))
  (datum->syntax #'invalid-context-rows
    (list 'quote
      (list
       (rejects #'(define-contextual-language bad #f #f #f
                    (rules (sum (left-tier (reference atom))))))
       (rejects #'(define-contextual-language bad #f #f #f
                    (rules (42 (literal "+")))))
       (rejects #'(define-contextual-language bad #f #f #f
                    (rules (sum (literal "+") (literal "-")))))))))

(def checked 0)
(def (context-check label actual expected)
  (unless (equal? actual expected)
    (error "context vocabulary mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "CONTEXT-CASE-OK: ") (display label) (newline) (force-output))

(context-check 'renamed-macro-and-host-values
  context-host-values '(host-sum host-product host-power))
(context-check 'malformed-rows-diagnostics (invalid-context-rows)
  '("left-tier requires an operand and operators"
    "expected a named rule with one expression"
    "expected a named rule with one expression"))
(for-each
 (lambda (source)
   (let ((candidate (parse-source contextual-study-parser source))
         (control (parse-source tier-study-parser source)))
     (context-check (list 'accepted source) (parse-artifact-success? candidate) #t)
     (context-check (list 'control-accepted source) (parse-artifact-success? control) #t)
     (context-check (list 'roundtrip source) (parse-artifact-roundtrip candidate) source)
     (context-check (list 'complete-event-parity source)
       (parse-artifact-events candidate) (parse-artifact-events control))))
 '("a" "42" "a-b-c" "1-2-3" "a - b - c" "a+b*c" "(a+b)*c"
   "-a*b" "a+-b" "--a" "a / b - c" " \n a + b \t" "a**b**c"))
(for-each
 (lambda (source)
   (context-check (list 'rejected source)
     (parse-artifact-success? (parse-source contextual-study-parser source)) #f))
 '("" "a+" "*a" "a b" "(a+b"))
(display "CONTEXT-RUNTIME-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
