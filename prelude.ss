;;; Public Parser language; native grammar syntax loads on use.
prelude: :gerbil/core
package: tao3k/parser
(import :gerbil/core
        (phi: +1 :gerbil/core)
        (phi: +2 :gerbil/core))
(export (import: :gerbil/core)
        (phi: +1 (import: :gerbil/core))
        (phi: +2 (import: :gerbil/core))
        deflanguage defgrammar-syntax deftext-profile)

;; Native imports are introduced only when a declaration is used. Preserve
;; both the caller's lexical scope and source position for the existing DSL.
;; Each public transformer passes its syntax identifier to the shared helper;
;; the helper projects only its name when constructing the native prefix.
(begin-syntax
  (def (parser-declaration stx name)
    (let (prefix (symbol->string (gensym 'parser-declaration-)))
      (with-syntax ((prefix-id (datum->syntax (stx-car stx) (string->symbol prefix)))
                    (entry (datum->syntax (stx-car stx)
                             (string->symbol (string-append prefix (symbol->string (stx-e name))))
                             (stx-source stx)))
                    (arguments (stx-cdr stx)))
        #'(begin
            (import (prefix-in :gerbil-parser/language-support/grammar prefix-id))
            (entry . arguments))))))
(defsyntax (deflanguage stx) (parser-declaration stx #'deflanguage))
(defsyntax (defgrammar-syntax stx) (parser-declaration stx #'defgrammar-syntax))
(defsyntax (deftext-profile stx) (parser-declaration stx #'deftext-profile))
