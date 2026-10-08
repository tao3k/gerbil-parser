;;; Native language context and descriptor catalogs shared by ABI publication.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in ../grammar/algebra grammar-expression-fields)
        (only-in ../language/descriptor language-grammar-grammar)
        (only-in ../language/source source-language? source-language-result-catalog))
(export #t)

(defstruct native-language
  (id descriptor parser syntax-kind-index terminal-index field-symbols field-index plan)
  transparent: #t)

(def (descriptor-section descriptor name)
  (if (and (source-language? descriptor) (eq? name 'rules)) '()
    (let (row (assq name (if (source-language? descriptor)
                          (source-language-result-catalog descriptor)
                          (language-grammar-grammar descriptor))))
      (unless row (error "native descriptor lacks a required result section" name))
      (cdr row))))

;; Opaque rejected source uses the engine token without changing recognition.
(def (native-terminal-rows descriptor)
  (let (rows (descriptor-section descriptor 'terminals))
    (if (assq 'unknown rows) rows
      (append rows '((unknown token))))))

(def (indexed-symbols symbols)
  (let (table (make-hash-table-eq size: (length symbols)))
    (for-each (lambda (symbol index) (hash-put! table symbol index))
              symbols
              (iota (length symbols)))
    table))

;; A shared field has one stable id. The grammar expression algebra is the
;; authority for fields emitted by the parser; declared syntax fields are
;; retained first for the descriptor's complete public surface.
(def (descriptor-field-symbols grammar)
  (delete-duplicates/hash
   (append
    (apply append (map caddr (descriptor-section grammar 'syntax-kinds)))
    (apply append
           (map (lambda (row) (grammar-expression-fields (cadr row)))
                (descriptor-section grammar 'rules))))
   from-end?: #t))

(def (make-native-language-context id grammar parser)
  (let (field-symbols (descriptor-field-symbols grammar))
    (make-native-language
     id grammar parser
     (indexed-symbols (map car (descriptor-section grammar 'syntax-kinds)))
     (indexed-symbols (map car (native-terminal-rows grammar)))
     field-symbols
     (indexed-symbols field-symbols) #f)))

