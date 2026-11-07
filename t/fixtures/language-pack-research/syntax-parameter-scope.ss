#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/stxparam "author-context"
        (rename-in "author-context" (@@author-declaration @@renamed-author)))
(defsyntax (context-value stx)
  (let (value (syntax-parameter-value #'@@author-declaration))
    (datum->syntax #'context-value (list 'quote (and value (syntax->datum value))))))
;; Experimental first-context policy; not a published declaration protocol.
(defsyntax (retain-author-context stx)
  (syntax-case stx ()
    ((_ candidate body ...)
     (with-syntax ((selected (or (syntax-parameter-value #'@@author-declaration)
                                #'candidate)))
       #'(syntax-parameterize ((@@author-declaration (quote-syntax selected)))
           body ...)))))
(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "syntax parameter scope mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "SYNTAX-PARAMETER-CASE-OK: ") (display label) (newline) (force-output))
(probe 'default (context-value) #f)
(probe 'scoped
  (syntax-parameterize ((@@author-declaration (quote-syntax (author declaration))))
    (context-value))
  '(author declaration))
(probe 'after-scope (context-value) #f)
(probe 'nested-nearest-and-restoration
  (syntax-parameterize ((@@author-declaration (quote-syntax (outer invocation))))
    (list (context-value)
          (syntax-parameterize ((@@author-declaration (quote-syntax (inner invocation))))
            (context-value))
          (context-value)))
  '((outer invocation) (inner invocation) (outer invocation)))
(probe 'after-nested (context-value) #f)
(probe 'retain-existing-author
  (syntax-parameterize ((@@author-declaration (quote-syntax (root invocation))))
    (retain-author-context (inner invocation) (context-value)))
  '(root invocation))
(probe 'retain-default-author
  (retain-author-context (new invocation) (context-value))
  '(new invocation))
(probe 'after-retention (context-value) #f)
(probe 'renamed-parameter-binding
  (syntax-parameterize ((@@renamed-author (quote-syntax (renamed invocation))))
    (context-value))
  '(renamed invocation))
(display "SYNTAX-PARAMETER-SCOPE-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
