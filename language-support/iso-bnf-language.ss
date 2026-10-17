;;; -*- Gerbil -*-
;;; Expansion-time ISO BNF compiler for runtime-only language packs.

(import (for-syntax :std/misc/ports
                    (only-in ../src/compiler/concise-language lower-source-rule-overlays lower-source-rule-precedences)
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
        :gerbil-parser/src/compiler/language-expander)
(export iso-bnf)

;;; ISO WG3 BNF uses prose for lexical character classes.  The source adapter
;;; lowers those named classes to the shared scanner token contract while all
;;; parser productions remain represented in the immutable Grammar IR.
;; compile-iso-bnf-language
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Lowers a pinned ISO WG3 BNF catalog through the stable language v1
;;       owner while preserving native production spelling and source lines.
;;
;;       # Examples
;;
;;       ```scheme
;;       (compile-iso-bnf-language language
;;         (identity "lang" "v1" "contract.v1")
;;         (reference "source-v1" "commit")
;;         (digest "sha256:...") (source "grammar.bnf")
;;         (entrypoint program) (conflicts reject) (case-insensitive #f))
;;       ;; => grammar, Bound IR, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: Runtime receives only immutable v1 data; native BNF names and
;;       source coordinates remain available in the Bound Grammar IR sidecar.
;;     %
(defsyntax (compile-iso-bnf-language stx)
  (syntax-case stx
      (identity reference digest source rule-overrides rule-precedences
                entrypoint conflicts
                case-insensitive)
    ((_ prefix (reference source-version source-commit) section ...)
     (identifier? #'prefix)
     (with-syntax ((language-name (datum->syntax #'prefix (symbol->string (syntax->datum #'prefix)))))
       #'(compile-iso-bnf-language prefix (identity language-name #f #f)
           (reference source-version source-commit) section ...)))
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
          (or (stx-string? #'version) (eq? (stx-e #'version) #f))
          (or (stx-string? #'contract) (eq? (stx-e #'contract) #f))
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
         #'(compile-language prefix
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
             (lineage compile-iso-bnf-language source-version source-commit
                      overlay-id ...)
             (flow (source lexical) (lexical cst))))))
    (_ (raise-syntax-error #f "invalid ISO BNF language declaration" stx))))


;;; Source-backed rules retain their checked upstream witness and provenance.
(defsyntax (iso-bnf stx)
  (syntax-case stx (syntax rules reference digest source rule-precedences entrypoint conflicts case-insensitive)
    ((_ prefix
        (syntax (reference version commit) (digest hash) (source path)
                (rule-precedences precedence ...)
                (entrypoint root) (conflicts policy) (case-insensitive insensitive))
        (rules overlay ...))
     (identifier? #'prefix)
     (with-syntax (((lowered ...) (lower-source-rule-overlays #'(overlay ...) stx #'prefix #'version #'commit))
                   ((lowered-precedence ...) (lower-source-rule-precedences #'(precedence ...) stx #'prefix #'version #'commit)))
       #'(compile-iso-bnf-language prefix
           (reference version commit) (digest hash) (source path)
           (rule-overrides lowered ...) (rule-precedences lowered-precedence ...)
           (entrypoint root) (conflicts policy) (case-insensitive insensitive))))
    (_ (raise-syntax-error #f "iso-bnf vocabulary requires checked source syntax and rule overlays" stx))))
