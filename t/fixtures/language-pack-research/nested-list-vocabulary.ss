;;; -*- Gerbil -*-
(import "located-component")
(export argument-list public-argument-list reconstructed-argument-list)

(defrules argument-list ()
  ((_ binding owner entry item separator label)
   (deflocated-list binding owner entry item separator (field label))))

(defrules public-argument-list ()
  ((_ binding owner entry item separator label)
   (argument-list binding owner entry item separator label)))

(defsyntax (reconstructed-argument-list stx)
  (syntax-case stx ()
    ((_ binding owner entry item separator label)
     ;; Negative control: retain definition bindings, discard argument syntax.
     (datum->syntax #'reconstructed-argument-list
       (list 'deflocated-list
         (syntax->datum #'binding) (syntax->datum #'owner)
         (syntax->datum #'entry) (syntax->datum #'item)
         (syntax->datum #'separator) (list 'field (syntax->datum #'label)))
       #f #f))))
