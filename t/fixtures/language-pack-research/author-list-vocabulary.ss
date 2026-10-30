;;; -*- Gerbil -*-
(import "located-component"
        (for-syntax "declaration-contracts"))
(export author-arguments)
(defsyntax (author-arguments stx)
  (syntax-case stx ()
    ((_ binding owner entry item separator label)
     (begin
       ;; This wrapper owns its positional label contract before delegation.
       (require-declaration-identifier #'label stx "author-arguments" "label")
       #'(deflocated-list binding owner entry item separator (field label))))
    (_ (raise-syntax-error #f
         "author-arguments: expected binding owner entry item separator label" stx))))
