;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import (only-in ../grammar/lexical-algebra expand-text-lexical-rows)
        ;; Retain the ordinary build dependency without executing its runtime
        ;; at interface-import phase. Invocation below requests that phase.
        (for-template (only-in ../compiler/language-declaration expand-language-declaration))
        (only-in ./descriptor make-language-grammar)
        (only-in ./assembly assemble-language-parser))
(export deflanguage
        defgrammar-syntax)

;; Reading an already compiled grammar must not initialize the compiler.
;; Resolve the service only when an author declaration actually expands. The
;; SDK owns context caching, initialization and failure propagation; the
;; returned syntax uses the runtime bindings supplied below, not service globals.
(begin-syntax
  (def (language-expansion-service)
    (let* ((context (import-module ':gerbil-parser/src/compiler/language-declaration #f #t))
           (entry (find (lambda (entry)
                          (and (zero? (module-export-phi entry))
                               (eq? (module-export-name entry) 'expand-language-declaration)))
                        (module-context-export context))))
      (unless entry
        (error "language compiler lacks its declaration expansion service"))
      (eval (binding-id (core-resolve-module-export entry))))))

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
     ((language-expansion-service)
      stx expand-text-lexical-rows
      #'assemble-language-parser #'make-language-grammar
      #'deflanguage #'begin #'def))
    ((_ binding (syntax (vocabulary clause ...)) (rules rule ...))
     (and (identifier? #'binding) (identifier? #'vocabulary))
     #'(vocabulary binding (syntax clause ...) (rules rule ...)))
    (_ (raise-syntax-error #f
         "deflanguage requires exactly one syntax vocabulary and one rules block; flat declarations and release identity are not author syntax"
         stx))))
