#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "located-component"
        (for-syntax (only-in :gerbil/expander core-expand1)))

(defsyntax (source-free-cases stx)
  (def (source-record tree)
    (cond
     ((and (pair? tree) (eq? (car tree) 'quote)
           (pair? (cdr tree)) (list? (cadr tree))
           (andmap pair? (cadr tree)) (assq 'authorOrigin (cadr tree))) (cadr tree))
     ((pair? tree) (or (source-record (car tree)) (source-record (cdr tree))))
     (else #f)))
  (def (fresh form)
    ;; Retain lexical context, deliberately omit reader source.
    (datum->syntax #'source-free-cases (syntax->datum form) #f #f))
  (let* ((valid (fresh #'(deflocated-list component call-arguments arguments
                         (reference name) (literal ",") (field argument))))
         (source (source-record (syntax->datum (core-expand1 valid))))
         (invalid (fresh #'(deflocated-list component 46 arguments
                           (reference name) (literal ",") (field argument))))
         (failure
          (with-catch
           (lambda (condition)
             (let (blame (car (slot-ref condition 'irritants)))
               (list (syntax-error? condition) (error-message condition)
                     (syntax->datum blame) (stx-source blame))))
           (lambda () (core-expand1 invalid) #f))))
    (datum->syntax #'source-free-cases (list 'quote (list source failure)))))

(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "declaration diagnostic mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "DECLARATION-DIAGNOSTIC-CASE-OK: ") (display label) (newline) (force-output))
(let* ((results (source-free-cases)) (source (car results)) (failure (cadr results)))
  (probe 'source-free-author (cdr (assq 'authorOrigin source)) #f)
  (probe 'source-free-path (cdr (assq 'path source)) #f)
  (probe 'source-free-location (cdr (assq 'location source)) #f)
  (probe 'source-free-owner (cdr (assq 'componentOwner source)) 'call-arguments)
  (probe 'source-free-template-known (pair? (cdr (assq 'templateOrigin source))) #t)
  (probe 'source-free-error failure
    '(#t "deflocated-list: owner must be an identifier" 46 #f)))
(display "SOURCE-FREE-DIAGNOSTICS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
