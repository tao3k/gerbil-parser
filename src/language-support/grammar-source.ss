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
        (for-syntax (only-in ./iso-bnf
                             iso-bnf-source-declaration-sources
                             iso-bnf-source-declaration-sources/overrides
                             iso-bnf-source-declaration-sources/precedences
                             iso-bnf-apply-rule-precedences
                             iso-bnf-source-grammar-rules
                             iso-bnf-source-grammar-rules/overrides
                             iso-bnf-source-literals
                             iso-bnf-source-syntax-kinds
                             parse-iso-bnf-source/expected))
        (only-in ./iso-bnf parse-iso-bnf-source/expected)
        (only-in ./antlr4-source antlr4-source-from-datum)
        (only-in ./javacc-source javacc-source-from-datum)
        :gerbil-parser/src/language/grammar)
(export defsyntax-iso-bnf-source
        defsyntax-antlr4-source
        defsyntax-javacc-source
        deflanguage-antlr4-grammar
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

;;; A language pack declares identity and pinned native grammar once; expansion
;;; materializes its catalog, Grammar IR, LALR table, and generated machine.
;; deflanguage-antlr4-grammar
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Lowers a pinned ANTLR4 grammar into the stable language v1 contract.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage-antlr4-grammar language
;;         (identity "lang" "v1" "contract.v1")
;;         (reference "upstream-v1" "commit")
;;         (digest "sha256:...") (source "grammar.g4")
;;         (entrypoint program) (conflicts reject) (case-insensitive #f))
;;       ;; => grammar, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: Runtime receives immutable v1 data and never provider source.
;;     %
(defsyntax (deflanguage-antlr4-grammar stx)
  (syntax-case stx
      (identity reference digest source entrypoint conflicts case-insensitive)
    ((_ prefix
        (identity language version contract)
        (reference source-version source-commit)
        (digest expected-digest)
        (source path)
        (entrypoint entry-name)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value))
     (and (identifier? #'prefix)
          (stx-string? #'language)
          (stx-string? #'version)
          (stx-string? #'contract)
          (stx-string? #'source-version)
          (stx-string? #'source-commit)
          (stx-string? #'expected-digest)
          (stx-string? #'path)
          (identifier? #'entry-name))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string))
            (catalog
             (parse-antlr4-source/expected
              (stx-e #'language) (stx-e #'source-version)
              (stx-e #'source-commit) (stx-e #'expected-digest) content))
            (syntax-rows
             (append
              (antlr4-source-parser-syntax-kinds catalog)
              '((LexicalIdentifier token (text))
                (NumericLiteralToken token (text))
                (StringLiteralToken token (text))
                (WhitespaceTrivia token (text))
                (CommentTrivia token (text))
                (PunctuationToken token (text))
                (UnknownToken token (text)))))
            (rule-rows (antlr4-source-parser-grammar-rules catalog))
            (literal-values (antlr4-source-parser-literals catalog))
            (source-map
             (antlr4-source-declaration-sources catalog (stx-e #'path))))
       (with-syntax ((((syntax-row ...)) (list syntax-rows))
                     (((rule-row ...)) (list rule-rows))
                     (((literal-value ...)) (list literal-values))
                     (source-map-value source-map))
         #'(deflanguage-grammar prefix
             (identity language version contract)
             (syntax-kinds syntax-row ...)
             (terminals
              (identifier LexicalIdentifier)
              (number NumericLiteralToken)
              (string StringLiteralToken)
              (whitespace WhitespaceTrivia)
              (comment CommentTrivia)
              (punctuation PunctuationToken)
              (unknown UnknownToken))
             (lexical-rules
              (whitespace (whitespace+))
              (comment (choice (line-comment "//")
                               (block-comment "/*" "*/")))
              (string (quoted-string "\"" "'" "`"))
              (number
               (number-literal ("0x" "0o" "0b") "_"
                               ("M" "m" "F" "f" "D" "d") #t #t))
              (identifier (identifier))
              (punctuation (literals literal-value ...))
              (unknown (fallback)))
             (rules rule-row ...)
             (extras whitespace comment)
             (keywords)
             (parser-entrypoints (entry-name parse pure))
             (recoveries
              (entry-name "GERBIL-PARSER-ANTLR4-SOURCE" preserve-source))
             (conflicts conflict-policy)
             (case-insensitive case-insensitive-value)
             (source-ownership source-map-value)
             (lineage deflanguage-antlr4-grammar source-version source-commit)
             (flow (source lexical) (lexical cst))))))
    (_ (raise-syntax-error #f "invalid ANTLR4 language declaration" stx))))

;;; ISO WG3 BNF uses prose for lexical character classes.  The source adapter
;;; lowers those named classes to the shared scanner token contract while all
;;; parser productions remain represented in the immutable Grammar IR.
;; deflanguage-iso-bnf-grammar
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Lowers a pinned ISO WG3 BNF catalog through the stable language v1
;;       owner while preserving native production spelling and source lines.
;;
;;       # Examples
;;
;;       ```scheme
;;       (deflanguage-iso-bnf-grammar language
;;         (identity "lang" "v1" "contract.v1")
;;         (reference "source-v1" "commit")
;;         (digest "sha256:...") (source "grammar.bnf")
;;         (entrypoint program) (conflicts reject) (case-insensitive #f))
;;       ;; => grammar, Bound IR, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: Runtime receives only immutable v1 data; native BNF names and
;;       source coordinates remain available in the Bound Grammar IR sidecar.
;;     %
(defsyntax (deflanguage-iso-bnf-grammar stx)
  (syntax-case stx
      (identity reference digest source rule-overrides rule-precedences
                entrypoint conflicts
                case-insensitive)
    ((_ prefix
        (identity language version contract)
        (reference source-version source-commit)
        (digest expected-digest)
        (source path)
        (entrypoint entry-name)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value))
     #'(deflanguage-iso-bnf-grammar prefix
         (identity language version contract)
         (reference source-version source-commit)
         (digest expected-digest)
         (source path)
         (rule-overrides)
         (rule-precedences)
         (entrypoint entry-name)
         (conflicts conflict-policy)
         (case-insensitive case-insensitive-value)))
    ((_ prefix
        (identity language version contract)
        (reference source-version source-commit)
        (digest expected-digest)
        (source path)
        (rule-overrides override-row ...)
        (entrypoint entry-name)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value))
     #'(deflanguage-iso-bnf-grammar prefix
         (identity language version contract)
         (reference source-version source-commit)
         (digest expected-digest)
         (source path)
         (rule-overrides override-row ...)
         (rule-precedences)
         (entrypoint entry-name)
         (conflicts conflict-policy)
         (case-insensitive case-insensitive-value)))
    ((_ prefix
        (identity language version contract)
        (reference source-version source-commit)
        (digest expected-digest)
        (source path)
        (rule-overrides override-row ...)
        (rule-precedences precedence-row ...)
        (entrypoint entry-name)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value))
     (and (identifier? #'prefix)
          (stx-string? #'language)
          (stx-string? #'version)
          (stx-string? #'contract)
          (stx-string? #'source-version)
          (stx-string? #'source-commit)
          (stx-string? #'expected-digest)
          (stx-string? #'path)
          (identifier? #'entry-name))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string))
            (catalog
             (parse-iso-bnf-source/expected
              (stx-e #'language) (stx-e #'source-version)
              (stx-e #'source-commit) (stx-e #'expected-digest) content))
            (syntax-rows
             (append
              (iso-bnf-source-syntax-kinds catalog)
              '((LexicalIdentifier token (text))
                (DelimitedIdentifierToken token (text))
                (NumericLiteralToken token (text))
                (StringLiteralToken token (text))
                (WhitespaceTrivia token (text))
                (CommentTrivia token (text))
                (PunctuationToken token (text))
                (UnknownToken token (text)))))
            (overrides (syntax->datum #'(override-row ...)))
            (precedences (syntax->datum #'(precedence-row ...)))
            (source-rule-rows
             (iso-bnf-source-grammar-rules/overrides catalog overrides))
            (rule-rows
             (iso-bnf-apply-rule-precedences source-rule-rows precedences))
            (literal-values
             (filter
              (lambda (literal)
                (and (> (string-length literal) 0)
                     (let (first (string-ref literal 0))
                       (not (or (char-alphabetic? first)
                                (char-numeric? first)
                                (char=? first #\_))))))
              (iso-bnf-source-literals catalog rule-rows)))
            (source-map
             (iso-bnf-source-declaration-sources/precedences
              (iso-bnf-source-declaration-sources/overrides
               catalog (stx-e #'path) overrides)
              source-rule-rows precedences))
            (overlay-lineage
             (append (map cadr overrides) (map cadr precedences))))
       (with-syntax ((((syntax-row ...)) (list syntax-rows))
                     (((rule-row ...)) (list rule-rows))
                     (((literal-value ...)) (list literal-values))
                     (((overlay-id ...)) (list overlay-lineage))
                     (source-map-value source-map))
         #'(deflanguage-grammar prefix
             (identity language version contract)
             (syntax-kinds syntax-row ...)
             (terminals
              (identifier LexicalIdentifier)
              (delimited-identifier DelimitedIdentifierToken)
              (number NumericLiteralToken)
              (string StringLiteralToken)
              (whitespace WhitespaceTrivia)
              (comment CommentTrivia)
              (punctuation PunctuationToken)
              (unknown UnknownToken))
             (lexical-rules
              (whitespace (whitespace+))
              (comment (choice (line-comment "//")
                               (block-comment "/*" "*/")))
              (delimited-identifier (quoted-string "`"))
              (string (quoted-string "\"" "'"))
              (number
               (number-literal ("0x" "0X" "0o") "_"
                               ("F" "f" "D" "d") #t #f))
              (identifier (identifier))
              (punctuation (literals literal-value ...))
              (unknown (fallback)))
             (rules rule-row ...)
             (extras whitespace comment)
             (keywords)
             (parser-entrypoints (entry-name parse pure))
             (recoveries
              (entry-name "GERBIL-PARSER-ISO-BNF-SOURCE" preserve-source))
             (conflicts conflict-policy)
             (case-insensitive case-insensitive-value)
             (source-ownership source-map-value)
             (lineage deflanguage-iso-bnf-grammar source-version source-commit
                      overlay-id ...)
             (flow (source lexical) (lexical cst))))))
    (_ (raise-syntax-error #f "invalid ISO BNF language declaration" stx))))
