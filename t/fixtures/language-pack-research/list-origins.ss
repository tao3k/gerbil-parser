;;; -*- Gerbil -*-
;;; Fixture-specific ownership map for unique merge/append contributions.
(import (only-in :clan/poo/object .ref .slot?)
        (only-in "../../../src/modules/parser/objects" grammar-role-name))
(export list-study-source-map list-study-row-source)
(def (ref value key) (let (row (assq key value)) (and row (cdr row))))
(def (list-study-source-map receipt)
  (list
   (cons 'rule
     (apply append
       (map
        (lambda (step)
          (let* ((owner (ref step 'role))
                 (rules (ref (ref step 'sections) 'rules))
                 (generated? (not (eq? owner 'list-study-base-role))))
            (map
             (lambda (name)
               (cons name
                 (list
                  (cons 'path "t/fixtures/language-pack-research/list-roles.ss")
                  (cons 'location
                    (string-append "role " (symbol->string owner)
                                   "; rule " (symbol->string name)))
                  (cons 'generated? generated?)
                  (cons 'componentOwner owner))))
             (or rules '()))))
        (ref receipt 'steps))))))

;;; Metadata for a known fixture occurrence, not an engine source resolver.
(def (list-study-row-source role index section row)
  (if (.slot? role 'row-source)
    ((.ref role 'row-source) section row)
  (let (owner (grammar-role-name role))
    (list (cons 'path "t/fixtures/language-pack-research/list-roles.ss")
          (cons 'location (string-append "role " (symbol->string owner)
                                        "; " (symbol->string section)
                                        " " (symbol->string (car row))))
          (cons 'generated? (not (eq? owner 'list-study-base-role)))))))
