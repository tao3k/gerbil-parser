;;; -*- Gerbil -*-
;;; Declarative public entry boundary for a versioned language parser.

(import ../runtime/parser)
(export deflanguage-parser-entry
        +language-parser-entry-schema-v1+
        language-parser-entry-ref)

(def +language-parser-entry-schema-v1+
  "gerbil-parser.language-entry.v1")

(def (language-parser-entry-ref entry key)
  (let (row (assq key entry))
    (and row (cdr row))))

(defrules deflanguage-parser-entry
  (identity machine parse)
  ((_ binding
      (identity language-value version-value contract-value)
      (machine parser-machine)
      (parse parse-binding))
   (begin
     (def binding
       (list
        (cons 'schema +language-parser-entry-schema-v1+)
        (cons 'language language-value)
        (cons 'version version-value)
        (cons 'contract contract-value)))
     (def (parse-binding source)
       (parse-source parser-machine source)))))
