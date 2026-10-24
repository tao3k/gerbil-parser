#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "located-component" "nested-list-vocabulary"
        (rename-in "nested-list-vocabulary" (public-argument-list imported-arguments))
        (for-syntax (only-in :gerbil/expander core-expand)))

(defsyntax (collect-nested-errors stx)
  (def (location syntax-value)
    (let (loc (stx-source syntax-value))
      (and loc
           (let (start (##position->filepos (##locat-start-position loc)))
             (list (##container->path (##locat-container loc))
                   (+ 1 (##filepos-line start)) (+ 1 (##filepos-col start)))))))
  (def (capture form)
    (with-catch
     (lambda (condition)
       (let* ((irritants (slot-ref condition 'irritants)) (blame (car irritants)))
         (list (syntax-error? condition) (error-message condition)
               (syntax->datum blame) (location blame)
               (syntax->datum (cadr irritants)))))
     (lambda () (core-expand form) #f)))
  (syntax-case stx ()
    ((_ form ...)
     (datum->syntax #'collect-nested-errors
       (list 'quote (stx-map capture #'(form ...)))))))

(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "nested diagnostic mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "NESTED-DIAGNOSTIC-CASE-OK: ") (display label) (newline) (force-output))
(def (line-at path number)
  (call-with-input-file path
    (lambda (port)
      (let loop ((index 1))
        (let (line (read-line port))
          (when (eof-object? line) (error "nested source line absent" path number))
          (if (= index number) line (loop (+ index 1))))))))
(for-each
 (lambda (result token source-present?)
   (probe (list 'typed token) (car result) #t)
   (probe (list 'message token) (cadr result)
     "deflocated-list: field label must be an identifier")
   (probe (list 'blame token) (caddr result) token)
   (let (loc (list-ref result 3))
     (probe (list 'source-preserved token) (pair? loc) source-present?)
     (if loc
       (let* ((line (line-at (car loc) (cadr loc))) (offset (- (caddr loc) 1))
              (text (number->string token)))
         (probe (list 'author-token token)
           (substring line offset (+ offset (string-length text))) text))
       (probe (list 'unknown-source token) loc #f)))
   ;; Retain this observation: the inner declaration, not the outer wrapper,
   ;; is the context supplied by the current component transformer.
   (probe (list 'inner-context token) (car (list-ref result 4)) 'deflocated-list))
 (collect-nested-errors
   (deflocated-list component call-arguments arguments
     (reference name) (literal ",") (field 61))
   (argument-list component call-arguments arguments
     (reference name) (literal ",") 62)
   (imported-arguments component call-arguments arguments
     (reference name) (literal ",") 63)
   (reconstructed-argument-list component call-arguments arguments
     (reference name) (literal ",") 64))
 '(61 62 63 64) '(#t #t #t #f))
(display "NESTED-DIAGNOSTICS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
