;;; -*- Gerbil -*-
;;; Declarative public entry boundary for a versioned language parser.

(import ../runtime/parser
        ./descriptor)
(export deflanguage-parser
        +language-parser-entry-schema-v1+
        language-parser-entry-ref)

(def +language-parser-entry-schema-v1+
  "gerbil-parser.language-entry.v1")

(def (language-parser-entry-ref entry key)
  (let (row (assq key entry))
    (and row (cdr row))))

(defrules deflanguage-parser
  (grammar parse)
  ((_ binding
      (grammar language-grammar-value)
      (parse parse-binding))
   (begin
     (def binding
       (list
        (cons 'schema +language-parser-entry-schema-v1+)
        (cons 'language (language-grammar-language language-grammar-value))
        (cons 'version (language-grammar-version language-grammar-value))
        (cons 'contract (language-grammar-contract language-grammar-value))))
     (def (parse-binding source)
       (parse-source (language-grammar-machine language-grammar-value)
                     source)))))
