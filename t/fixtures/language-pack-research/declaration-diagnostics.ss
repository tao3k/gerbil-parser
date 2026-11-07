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
(let* ((results (source-free-cases)) (source (car results)) (failure (cadr results)))
  (probe 'source-free-author (cdr (assq 'authorOrigin source)) #f)
  (probe 'source-free-path (cdr (assq 'path source)) #f)
  (probe 'source-free-location (cdr (assq 'location source)) #f)
  (probe 'source-free-owner (cdr (assq 'componentOwner source)) 'call-arguments)
  (probe 'source-free-template-known (pair? (cdr (assq 'templateOrigin source))) #t)
  (probe 'source-free-error failure
    '(#t "deflocated-list: owner must be an identifier" 46 #f)))
(display "DECLARATION-DIAGNOSTICS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
