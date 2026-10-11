;;; -*- Gerbil -*-
(import "located-component" "list-origins"
        (only-in "list-roles" list-study-base-role)
        (only-in "../../../src/modules/parser/objects" make-grammar))
(export make-located-list-grammar located-list-source-map)

(deflocated-list arguments-component call-arguments arguments
  (reference name) (literal ",") (field argument))
(deflocated-list elements-component array-elements elements
  (reference name) (literal ",") (field element))

(def (ref value key) (let (row (assq key value)) (and row (cdr row))))
(def (make-located-list-grammar)
  (make-grammar 'list-study '() (list list-study-base-role)
    (list (cons 'append (ref arguments-component 'role))
          (cons 'append (ref elements-component 'role)))))

;;; Replace component source entries; retain base-rule descriptive sources.
(def (located-list-source-map receipt)
  (let ((sources (cdr (assq 'rule (list-study-source-map receipt))))
        (origins (list (ref arguments-component 'source) (ref elements-component 'source))))
    (list
     (cons 'rule
       (map (lambda (row)
              (let (origin (find (lambda (value)
                                  (eq? (ref value 'componentOwner)
                                       (ref (cdr row) 'componentOwner))) origins))
                (if origin (cons (car row) origin) row)))
            sources)))))
