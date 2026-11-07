;;; -*- Gerbil -*-
;;; Test release identity belongs outside the author grammar declaration.
(import (only-in :gerbil-parser/src/language/descriptor language-grammar-with-identity))
(export bind-fixture-grammar-release)

(defsyntax (bind-fixture-grammar-release stx)
  (syntax-case stx ()
    ((_ prefix language version contract)
     (identifier? #'prefix)
     (with-syntax
       ((definition (datum->syntax #'prefix
                      (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-syntax"))))
        (released (datum->syntax #'prefix
                    (string->symbol (string-append (symbol->string (syntax->datum #'prefix)) "-language-grammar")))))
       #'(def released (language-grammar-with-identity definition language version contract))))
    (_ (raise-syntax-error #f "expected fixture prefix and release identity" stx))))
