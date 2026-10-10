;;; Closed canonical declarations and their independent source locations.
(export bound-source-catalog)

(def (bound-source-catalog width)
  (let* ((names (map (lambda (index) (string->symbol (string-append "Kind" (number->string index))))
                     (iota width)))
         (source (lambda (name) (list (cons 'path (symbol->string name))
                                      (cons 'generated? #f))))
         (source-order (reverse names))
         (grammar
          (list (cons 'schema "gerbil-parser.grammar-ir.v1")
                (cons 'grammar 'bound-source-catalog)
                (cons 'syntax-kinds (map (lambda (name) (list name 'node '(value))) names))
                (cons 'terminals '()) (cons 'lexical-rules '()) (cons 'rules '())))
         (sources
          (list (cons 'syntax-kind (map (lambda (name) (cons name (source name))) source-order))
                (cons 'field (map (lambda (name) (cons (list name 'value) (source name)))
                                  source-order)))))
    (values grammar sources)))
