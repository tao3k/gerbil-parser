;;; -*- Gerbil -*-
;;; Declarative public entry boundary for a versioned language parser.

(import (only-in ../runtime/parser parse-source)
        (only-in ./descriptor
                 language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version))
(export deflanguage-parser
        +language-parser-entry-schema+
        language-parser-entry-ref
        parse-language-source)

(def +language-parser-entry-schema+
  "gerbil-parser.language-entry.v1")

;; language-parser-entry-ref
;; : (-> LanguageParserEntry Symbol Datum)
(def (language-parser-entry-ref entry key)
  (let (row (assq key entry))
    (and row (cdr row))))

;;; Parse through one immutable LanguageGrammar configuration. Observability is
;;; therefore request-local by construction and cannot leak into another
;;; language, grammar version, or concurrent parse.
(def (parse-language-source language-grammar source)
  (parse-source
   (language-grammar-machine language-grammar)
   source
   (language-grammar-observability language-grammar)))

;;; This macro is the sole public projection from a versioned language
;;; descriptor to its runtime parse entry, keeping identity and machine bound.
;; deflanguage-parser
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `deflanguage-parser` expands an immutable language entry and parse binding.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage-parser parse-v1 (grammar grammar-v1) (parse parse-v1-source))
;;       ;; => version-bound parser entry and procedure
;;       ```
;;     %
(defrules deflanguage-parser
  (grammar parse)
  ((_ binding
      (grammar language-grammar-value)
      (parse parse-binding))
   (begin
     (def binding
       (list
        (cons 'schema +language-parser-entry-schema+)
        (cons 'language (language-grammar-language language-grammar-value))
        (cons 'version (language-grammar-version language-grammar-value))
        (cons 'contract (language-grammar-contract language-grammar-value))))
     (def (parse-binding source)
       (parse-language-source language-grammar-value source)))))
