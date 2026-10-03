;;; -*- Gerbil -*-
;;; Expansion-time ISO BNF compiler for runtime-only language packs.

(import (for-syntax :std/misc/ports
                    (only-in ./iso-bnf
                             iso-bnf-source-declaration-sources
                             iso-bnf-source-declaration-sources/overrides
                             iso-bnf-source-declaration-sources/precedences
                             iso-bnf-apply-rule-precedences
                             iso-bnf-source-grammar-rules
                             iso-bnf-source-grammar-rules/overrides
                             iso-bnf-source-literals
                             iso-bnf-source-syntax-kinds
                             parse-iso-bnf-source/expected))
        :gerbil-parser/src/language/grammar)
(export deflanguage-iso-bnf-grammar)

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

