;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import (only-in ../grammar/lexical-algebra expand-text-lexical-rows)
        (for-syntax (only-in ../compiler/parser-ir compile-parser)
                    (only-in ../compiler/bound-ir bind-grammar-ir)
                    (only-in ../compiler/language-artifact
                             expand-language-grammar-syntax)
                    (only-in ../compiler/concise-language expand-concise-language-syntax))
        (only-in ../compiler/machine install-parser-machine-backends!)
        (only-in ./descriptor make-language-grammar)
        (only-in ./assembly assemble-language-parser)
        (only-in ./source declare-source-language declare-source-syntax))
(export deflanguage
        deflanguage-grammar
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

;;; Owns validation and expansion of the complete versioned language declaration.
;;; Generated grammar, IR, and machine bindings share one expansion-time identity.
;; deflanguage-grammar
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `deflanguage-grammar` expands a language declaration into immutable bindings.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage-grammar example-v1 ...)
;;       ;; => grammar, parser IR, and parser machine bindings
;;       ```
;;     %
(defsyntax (deflanguage-grammar stx)
  (expand-language-grammar-syntax
   stx compile-parser bind-grammar-ir
        #'assemble-language-parser #'make-language-grammar #'begin #'def))

;;; Concise v1 authoring projection.  It infers terminal rows, syntax-kind
;;; rows, rule/token references, the single parser entry, and the connected
;;; parser flow before submitting the compiler declaration. Both public entries
;;; share one canonical admission, compilation and publication owner.
;; deflanguage
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `deflanguage` submits one hygienic language declaration directly to
;;       the shared canonical declaration compiler and stable v1 products.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage arithmetic
;;         (identity "arithmetic" "v1" "arithmetic-expression.v1")
;;         (root source-file)
;;         (lex (number Number (number)))
;;         (rules (source-file (node SourceFile (field value number))))
;;         (extras) (keywords) (recoveries)
;;         (conflicts reject) (case-insensitive #f))
;;       ;; => arithmetic grammar, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: every generated binding retains the stable v1 schemas.
;;     %
(defsyntax (deflanguage stx)
  (syntax-case stx (identity syntax rules root)
    ((_ binding (syntax (vocabulary clause ...)) (rules rule ...))
     (identifier? #'binding)
     #'(def binding (declare-source-syntax (vocabulary clause ... (rules rule ...)))))
    ((_ prefix (root root-name) section ...)
     (identifier? #'prefix)
     #'(deflanguage prefix (identity #f #f #f) (root root-name) section ...))
    ((_ binding (identity language version contract)
        (syntax (vocabulary clause ...)) (rules rule ...))
     (identifier? #'binding)
     #'(def binding
         (declare-source-language language version contract
           (vocabulary clause ... (rules rule ...)))))
    (_ (expand-concise-language-syntax
        stx expand-text-lexical-rows compile-parser bind-grammar-ir
   #'assemble-language-parser #'make-language-grammar #'install-parser-machine-backends!
        #'deflanguage #'begin #'def #'list))))
