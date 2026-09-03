;;; -*- Gerbil -*-
;;; Declarative expansion-time inclusion of external grammar sources.

(import (for-syntax :std/misc/ports)
        ./iso-bnf)
(export defsyntax-iso-bnf-source)

(defsyntax (defsyntax-iso-bnf-source stx)
  (syntax-case stx (identity digest source)
    ((_ binding
        (identity language version commit)
        (digest expected-digest)
        (source path))
     (and (identifier? #'binding) (stx-string? #'path))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string)))
       (with-syntax ((grammar-content content))
         #'(def binding
             (parse-iso-bnf-source/expected
              language version commit expected-digest grammar-content)))))
    (_ (raise-syntax-error #f "invalid ISO BNF source declaration" stx))))
