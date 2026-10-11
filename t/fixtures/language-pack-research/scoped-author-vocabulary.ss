;;; -*- Gerbil -*-
(import :std/stxparam "author-context" "located-component"
        (for-syntax "declaration-contracts"))
(export scoped-arguments)
(defsyntax (scoped-arguments stx)
  (syntax-case stx ()
    ((_ binding owner entry item separator label)
     (begin
       (require-declaration-identifier #'binding stx "scoped-arguments" "binding")
       (with-syntax ((author-form stx))
         ;; The outer declaration owns the public binding. The scoped body
         ;; constructs and returns a value through a hygienic local binding.
         #'(def binding
             (syntax-parameterize
               ((@@author-declaration (quote-syntax author-form)))
               (deflocated-list component-value owner entry item separator (field label))
               component-value)))))
    (_ (raise-syntax-error #f
         "scoped-arguments: expected binding owner entry item separator label" stx))))
