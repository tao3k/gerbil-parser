;;; HCL contextual product shared by integration tests and matched parse batches.
(import (only-in :gerbil-parser/src/compiler/machine parser-machine-ir)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-parser-ir compile-contextual-parser))
(export contextual-product)

(def (method name form result)
  (make-contextual-method name 'any 'any form result))

(def (rule name form matcher rank)
  (make-contextual-scan-rule name 'normal form matcher rank 'keep))

(def (contextual-product machine grammar-digest
                         (positions '(line))
                         (clauses '((line () ())))
                         (number-literal "1"))
  (let (role (make-contextual-role
              'hcl-parser-fixture
              (list (method 'identifier 'identifier 'identifier)
                    (method 'number 'number 'number)
                    (method 'punctuation 'punctuation 'punctuation)
                    (method 'space 'space 'horizontal-whitespace)
                    (method 'newline 'newline 'newline))))
    (compile-contextual-parser
     (parser-machine-ir machine) grammar-digest (list role)
     '(normal) positions
     '(identifier number punctuation space newline)
     (list (rule 'identifier 'identifier '(identifier) 0)
           (rule 'number 'number (list 'literal number-literal) 0)
           (rule 'punctuation 'punctuation '(literal "=") 0)
           (rule 'space 'space '(horizontal-whitespace+) 0)
           (rule 'newline 'newline '(newline-one) 0))
     'normal clauses)))

