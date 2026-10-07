;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import (only-in ../grammar/lexical-algebra expand-text-lexical-rows)
        (for-syntax (only-in ../compiler/parser-ir compile-parser)
                    (only-in ../compiler/bound-ir bind-grammar-ir)
                    (only-in ../compiler/concise-language expand-lexical-language-syntax))
        (only-in ./descriptor make-language-grammar)
        (only-in ./assembly assemble-language-parser))
(export deflanguage
        defgrammar-syntax)

;;; Defines an ordinary hygienic Gerbil macro whose expansion is consumed only
;;; by deflanguage's compile-time grammar-expression resolver.
;; defgrammar-syntax
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Defines a compile-time-only hygienic grammar expression macro.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgrammar-syntax (named token) (node Name (field value token)))
;;       ;; => syntax binding consumed by deflanguage expansion
;;       ```
;;     %
(defrules defgrammar-syntax ()
  ((_ (name argument ...) template)
   (defrules name () ((_ argument ...) template))))

;;; One author contract for all vocabularies. Vocabulary transformers receive
;;; the complete declaration; they cannot silently accept the retired flat form.
(defsyntax (deflanguage stx)
  (when (and (getenv "GERBIL_PARSER_LR_TRACE" #f) (stx-pair? (stx-cdr stx)))
    (displayln "AUTHOR-DECLARATION " (syntax->datum (stx-car (stx-cdr stx))))
    (force-output))
  (syntax-case stx (syntax rules lexical)
    ((_ prefix (syntax (lexical clause ...)) (rules rule ...))
     (identifier? #'prefix)
     (expand-lexical-language-syntax
      stx expand-text-lexical-rows compile-parser bind-grammar-ir
      #'assemble-language-parser #'make-language-grammar
      #'deflanguage #'begin #'def))
    ((_ binding (syntax (vocabulary clause ...)) (rules rule ...))
     (and (identifier? #'binding) (identifier? #'vocabulary))
     #'(vocabulary binding (syntax clause ...) (rules rule ...)))
    (_ (raise-syntax-error #f
         "deflanguage requires exactly one syntax vocabulary and one rules block; flat declarations and release identity are not author syntax"
         stx))))
