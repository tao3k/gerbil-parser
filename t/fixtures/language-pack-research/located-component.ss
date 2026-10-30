;;; -*- Gerbil -*-
;;; Source locations come from reader syntax, not a hand-written line table.
(import :std/stxparam "author-context" "reader-origin" (for-syntax "declaration-contracts") (only-in "list-roles" make-nonempty-list-role)
        (only-in "../../../src/grammar/algebra" grammar-expression))
(export deflocated-list)

(with-reader-origin template-definition-origin
  (defsyntax (deflocated-list stx)
    (def (position value)
      (and value
           (let (filepos (##position->filepos value))
             (list (cons 'line (+ 1 (##filepos-line filepos)))
                   (cons 'column (+ 1 (##filepos-col filepos)))))))
    (def (origin syntax-value)
      (let (location (stx-source syntax-value))
        (and location
             (list (cons 'path (##container->path (##locat-container location)))
                   (cons 'start (position (##locat-start-position location)))
                   (cons 'end (position (##locat-end-position location)))))))
    (def author-declaration (syntax-parameter-value #'@@author-declaration))
    (syntax-case stx (field)
      ((_ binding owner entry item separator (field label))
        (begin
          (require-declaration-identifier #'binding stx "deflocated-list" "binding" author-declaration)
          (require-declaration-identifier #'owner stx "deflocated-list" "owner" author-declaration)
          (require-declaration-identifier #'entry stx "deflocated-list" "entry" author-declaration)
          (require-declaration-identifier #'label stx "deflocated-list" "field label" author-declaration)
          (let* ((author (origin #'owner))
                 (template template-definition-origin)
                 (source
                  (list (cons 'path (and author (cdr (assq 'path author))))
                        (cons 'location author)
                        (cons 'generated? #t)
                        (cons 'componentOwner (syntax->datum #'owner))
                        (cons 'authorOrigin author)
                        (cons 'templateOrigin template))))
            (with-syntax ((source-value (datum->syntax #'binding (list 'quote source))))
              #'(def binding
                  (list
                   (cons 'role
                     (make-nonempty-list-role 'owner 'entry
                       (grammar-expression item) (grammar-expression separator) 'label))
                   (cons 'source source-value)))))))
      (_ (raise-syntax-error #f "deflocated-list: expected binding owner entry item separator (field label)" stx)))))
