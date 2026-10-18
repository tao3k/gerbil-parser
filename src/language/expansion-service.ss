;;; SDK-owned author service resolution, shared by every grammar vocabulary.
(import (only-in :gerbil/core/expander free-identifier=? identifier?)
        (only-in :gerbil/expander import-module module-context-export
                 module-export-phi module-export-name core-resolve-module-export
                 binding-id eval-syntax))
(export resolve-language-expansion language-expansion-tags?)

;; Literal identifiers are operands too: moving a matcher across phases must
;; not compare against newly bound identifiers in the service's own context.
(def (language-expansion-tags? actual expected)
  (and (= (length actual) (length expected))
       (andmap values
         (map (lambda (actual expected)
                (and (identifier? actual) (identifier? expected)
                     (free-identifier=? actual expected))) actual expected))))

(def (resolve-language-expansion module name)
  (let* ((context (import-module module #f #t))
         (entry (find (lambda (entry)
                        (and (zero? (module-export-phi entry))
                             (eq? (module-export-name entry) name)))
                      (module-context-export context))))
    (unless entry (error "missing language expansion service" module name))
    (let (expand (eval-syntax (binding-id (core-resolve-module-export entry))))
      (unless (procedure? expand) (error "invalid language expansion service" module name))
      expand)))
