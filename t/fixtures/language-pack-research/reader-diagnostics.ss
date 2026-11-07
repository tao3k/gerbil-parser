#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "located-component"
        (for-syntax (only-in :gerbil/expander core-expand1)))

(defsyntax (diagnostic-cases stx)
  (def (position value)
    (and value
         (let (filepos (##position->filepos value))
           (list (+ 1 (##filepos-line filepos)) (+ 1 (##filepos-col filepos))))))
  (def (location value)
    (let (loc (stx-source value))
      (and loc (list (##container->path (##locat-container loc))
                     (position (##locat-start-position loc))))))
  (def (capture form)
    (with-catch
     (lambda (condition)
       ;; SyntaxError extends Exception, not Error. Use its actual slots.
       (let* ((irritants (slot-ref condition 'irritants))
              (blame (car irritants)))
         (list (syntax-error? condition) (error-message condition)
               (syntax->datum blame) (location blame)
               (syntax->datum (cadr irritants)) (location (cadr irritants)))))
     (lambda () (core-expand1 form) #f)))
(syntax-case stx ()
  ((_ form ...)
   (datum->syntax #'diagnostic-cases
     (list 'quote (stx-map capture #'(form ...)))))))

(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "declaration diagnostic mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "DECLARATION-DIAGNOSTIC-CASE-OK: ") (display label) (newline) (force-output))
(def (line-at path number)
  (call-with-input-file path
    (lambda (port)
      (let loop ((index 1))
        (let (line (read-line port))
          (when (eof-object? line) (error "diagnostic source line absent" path number))
          (if (= index number) line (loop (+ index 1))))))))
(def messages
  '("deflocated-list: binding must be an identifier"
    "deflocated-list: owner must be an identifier"
    "deflocated-list: entry must be an identifier"
    "deflocated-list: field label must be an identifier"))
(for-each
 (lambda (result message token)
   (probe (list 'typed token) (car result) #t)
   (probe (list 'message token) (cadr result) message)
   (probe (list 'blame token) (caddr result) token)
   (let* ((primary (list-ref result 3))
          (loc (or primary (list-ref result 5))) (start (cadr loc))
          (line (line-at (car loc) (car start)))
          (offset (- (cadr start) 1))
          (text (if (pair? token)
                    (string-append "(" (number->string (car token)) ")")
                    (number->string token))))
     (probe (list 'parameter-source-available token) (pair? primary) #t)
     (probe (list 'effective-reader-coordinate token)
       (substring line offset (+ offset (string-length text))) text))
   (probe (list 'declaration-context token)
     (car (list-ref result 4)) 'deflocated-list))
 (diagnostic-cases
   (deflocated-list 42 call-arguments arguments
     (reference name) (literal ",") (field argument))
   (deflocated-list component 43 arguments
     (reference name) (literal ",") (field argument))
   (deflocated-list component call-arguments 44
     (reference name) (literal ",") (field argument))
   (deflocated-list component call-arguments arguments
     (reference name) (literal ",") (field 45))
   (deflocated-list (42) call-arguments arguments
     (reference name) (literal ",") (field argument))
   (deflocated-list component (43) arguments
     (reference name) (literal ",") (field argument))
   (deflocated-list component call-arguments (44)
     (reference name) (literal ",") (field argument))
   (deflocated-list component call-arguments arguments
     (reference name) (literal ",") (field (45)))) (append messages messages) '(42 43 44 45 (42) (43) (44) (45)))
(display "READER-DIAGNOSTICS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
