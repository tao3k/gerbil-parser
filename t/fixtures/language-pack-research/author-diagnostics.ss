#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "located-component" "author-list-vocabulary"
        (rename-in "author-list-vocabulary" (author-arguments imported-author-arguments))
        (for-syntax (only-in :gerbil/expander core-expand core-expand1)))

(defsyntax (collect-author-errors stx)
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
     (datum->syntax #'collect-author-errors
       (list 'quote (stx-map capture #'(form ...)))))))

(defsyntax (valid-author-expansion stx)
  (syntax-case stx ()
    ((_ outer explicit)
     (let (expanded (core-expand1 #'outer))
       (syntax-case expanded (field)
         ((target binding owner entry item separator (field label))
          (datum->syntax #'valid-author-expansion
            (list 'quote
              (list (free-identifier=? #'target #'deflocated-list)
                    (equal? (syntax->datum expanded) (syntax->datum #'explicit))
                    (equal? (stx-source #'label)
                            (stx-source (stx-list-ref #'outer 6))))))))))))

(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "author diagnostic mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "AUTHOR-DIAGNOSTIC-CASE-OK: ") (display label) (newline) (force-output))
(def (line-at path number)
  (call-with-input-file path
    (lambda (port)
      (let loop ((index 1))
        (let (line (read-line port))
          (when (eof-object? line) (error "nested source line absent" path number))
          (if (= index number) line (loop (+ index 1))))))))
(for-each
 (lambda (result token message context-head)
   (probe (list 'typed token) (car result) #t)
   (probe (list 'message token) (cadr result) message)
   (probe (list 'blame token) (caddr result) token)
   (let* ((loc (list-ref result 3))
          (line (line-at (car loc) (cadr loc))) (offset (- (caddr loc) 1))
          (text (number->string token)))
     (probe (list 'author-token token)
       (substring line offset (+ offset (string-length text))) text))
   (probe (list 'declaration-context token) (car (list-ref result 4)) context-head))
 (collect-author-errors
   (author-arguments component call-arguments arguments
     (reference name) (literal ",") 81)
   (imported-author-arguments component call-arguments arguments
     (reference name) (literal ",") 82)
   ;; Outer label validation wins even though owner is also invalid.
   (author-arguments component 83 arguments
     (reference name) (literal ",") 84)
   ;; Valid outer label delegates invalid owner to the existing component.
   (author-arguments component 85 arguments
     (reference name) (literal ",") argument)
   ;; The direct component uses its existing owner-before-label order.
   (deflocated-list component 86 arguments
     (reference name) (literal ",") (field 87)))
 '(81 82 84 85 86)
 '("author-arguments: label must be an identifier"
   "author-arguments: label must be an identifier"
   "author-arguments: label must be an identifier"
   "deflocated-list: owner must be an identifier"
   "deflocated-list: owner must be an identifier")
 '(author-arguments imported-author-arguments author-arguments deflocated-list deflocated-list))
(let (results
       (valid-author-expansion
         (author-arguments valid-component call-arguments arguments
           (reference name) (literal ",") argument)
         (deflocated-list valid-component call-arguments arguments
           (reference name) (literal ",") (field argument))))
  (probe 'valid-target-binding (car results) #t)
  (probe 'valid-expansion-datum (cadr results) #t)
  (probe 'valid-label-reader-source (caddr results) #t))
(display "AUTHOR-DIAGNOSTICS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
