;;; Inert public boundary declarations with native POO admission.
(import (only-in :clan/poo/object .o)
        ./source-boundary-types)
(export make-source-block-boundary make-source-named-boundary make-source-boundary-parent)
(def (admit predicate value)
  (unless (predicate value) (error "invalid source boundary declaration" value))
  value)
(def (make-source-block-boundary block-value heading-value)
  (admit source-boundary-query?
    (.o kind: 'source-boundary-query mode: 'fixed block: block-value heading: heading-value
        opening: #f closing: #f suffix: "" terminator: #f
        indent: #t heading-bound: #t case-insensitive: #t)))
(def (make-source-named-boundary opening-value closing-value suffix-value heading-value
                               (terminator-value #f) (indent? #t)
                               (heading-bound? #t) (case-insensitive? #t))
  (admit source-boundary-query?
    (.o kind: 'source-boundary-query mode: 'named block: #f heading: heading-value
        opening: opening-value closing: closing-value suffix: suffix-value terminator: terminator-value
        indent: indent? heading-bound: heading-bound? case-insensitive: case-insensitive?)))
(def (make-source-boundary-parent id-value closing-value (name-start-value #f) (name-end-value #f)
                                 (suffix-value "") (case-insensitive? #t))
  (admit source-boundary-parent?
    (.o kind: 'source-boundary-parent id: id-value closing: closing-value name-start: name-start-value
        name-end: name-end-value suffix: suffix-value case-insensitive: case-insensitive?)))
