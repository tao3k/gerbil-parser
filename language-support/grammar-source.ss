;;; -*- Gerbil -*-
;;; Declarative expansion-time inclusion of external grammar sources.

(import (for-syntax :std/misc/ports
                    (only-in ./antlr4-source
                             antlr4-source->datum
                             antlr4-source-declaration-sources
                             antlr4-source-parser-grammar-rules
                             antlr4-source-parser-literals
                             antlr4-source-parser-syntax-kinds
                             parse-antlr4-source/expected))
        (for-syntax (only-in ./javacc-source
                             javacc-source->datum
                             parse-javacc-source/expected))
        (only-in ./iso-bnf parse-iso-bnf-source/expected)
        (only-in ./antlr4-source antlr4-source-from-datum)
        (only-in ./javacc-source javacc-source-from-datum)
        (only-in ./iso-bnf-language deflanguage-iso-bnf-grammar))
(export defsyntax-iso-bnf-source
        defsyntax-antlr4-source
        defsyntax-javacc-source
        deflanguage-iso-bnf-grammar)

;;; JavaCC source is admitted and reduced to an immutable production inventory
;;; during expansion.  Generated Java and Java action blocks never enter the
;;; runtime authority.
(defsyntax (defsyntax-javacc-source stx)
  (syntax-case stx (identity digest source)
    ((_ binding
        (identity language version commit)
        (digest expected-digest)
        (source path))
     (and (identifier? #'binding)
          (stx-string? #'language)
          (stx-string? #'version)
          (stx-string? #'commit)
          (stx-string? #'expected-digest)
          (stx-string? #'path))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string))
            (catalog
             (parse-javacc-source/expected
              (stx-e #'language) (stx-e #'version) (stx-e #'commit)
              (stx-e #'expected-digest) content))
            (catalog-data (javacc-source->datum catalog)))
       (with-syntax ((materialized-catalog catalog-data))
         #'(def binding
             (javacc-source-from-datum 'materialized-catalog)))))
    (_ (raise-syntax-error #f "invalid JavaCC source declaration" stx))))

;;; Boundary: all filesystem reads and native grammar parsing happen during
;;; expansion; published runtime values are immutable, digest-bound data.
;; defsyntax-iso-bnf-source
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Embeds and validates one pinned ISO BNF source catalog.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defsyntax-iso-bnf-source catalog
;;         (identity "lang" "v1" "commit")
;;         (digest "sha256:...") (source "grammar.bnf"))
;;       ;; => immutable ISO BNF catalog binding
;;       ```
;;       Result: expansion fails closed when the source digest differs.
;;     %
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

;;; ANTLR text is parsed and digest-checked by the expander; runtime code only
;;; reconstructs the immutable typed catalog and never reparses native source.
;; defsyntax-antlr4-source
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Embeds and validates one pinned ANTLR4 source catalog.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defsyntax-antlr4-source catalog
;;         (identity "lang" "v1" "commit")
;;         (digest "sha256:...") (source "grammar.g4"))
;;       ;; => immutable ANTLR4 catalog binding
;;       ```
;;       Result: runtime materialization uses only expansion-produced data.
;;     %
(defsyntax (defsyntax-antlr4-source stx)
  (syntax-case stx (identity digest source)
    ((_ binding
        (identity language version commit)
        (digest expected-digest)
        (source path))
     (and (identifier? #'binding)
          (stx-string? #'language)
          (stx-string? #'version)
          (stx-string? #'commit)
          (stx-string? #'expected-digest)
          (stx-string? #'path))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string))
            (catalog
             (parse-antlr4-source/expected
              (stx-e #'language) (stx-e #'version) (stx-e #'commit)
              (stx-e #'expected-digest) content))
            (catalog-data (antlr4-source->datum catalog)))
       (with-syntax ((materialized-catalog catalog-data))
         #'(def binding
             (antlr4-source-from-datum 'materialized-catalog)))))
    (_ (raise-syntax-error #f "invalid ANTLR4 source declaration" stx))))
