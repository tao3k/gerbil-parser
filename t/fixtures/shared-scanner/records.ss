;;; Independent language pack: normal descriptor + declarative scanner rows.
(import (only-in ../rowan-record-assignments/languages/records/grammar records-language-grammar)
        (for-syntax
         (only-in ../rowan-record-assignments/languages/records/grammar records-language-grammar records-parser-ir)
         (only-in :gerbil-parser/src/compiler/contextual-parser-ir compile-contextual-parser/declaration)
         (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
         (only-in :gerbil-parser/src/compiler/machine parser-machine-grammar-digest)))
(export records-language-grammar records-contextual-product)
(defsyntax (records-contextual stx)
  (datum->syntax #'records-contextual
   (list 'quote (compile-contextual-parser/declaration
   records-parser-ir (parser-machine-grammar-digest (language-grammar-machine records-language-grammar))
   '((records ((id any any id identifier) (one any any one number)
               (eq any any eq punctuation) (nl any any nl newline)
               (ws any any ws whitespace) (str any any str string))))
   '(main) '(name value) '(id one eq nl ws str)
   '((id main id (identifier) 0 keep)
     (one main one (literal "1") 0 keep)
     (eq main eq (literal "=") 0 keep)
     (nl main nl (newline-one) 0 keep)
     (ws main ws (horizontal-whitespace+) 0 keep)
     (str main str (quoted-string ("\"")) 0 keep))
   'main '((name ((token identifier)) ()) (value () ()))))))
(def records-contextual-product (records-contextual))
