;;; Author-time ISO BNF service; runtime expansion operands retain caller scope.
(import (only-in :gerbil/core/expander
                 syntax-case syntax with-syntax datum->syntax syntax->datum syntax->list
                 stx-string? stx-e stx-source stx-map identifier? raise-syntax-error)
        (only-in ../language/expansion-service language-expansion-tags?)
        (only-in :std/misc/ports read-all-as-string)
        (only-in ./concise-language lower-source-rule-overlays lower-source-rule-precedences)
        (only-in ../../language-support/iso-bnf iso-bnf-source-declaration-sources
                 iso-bnf-source-declaration-sources/overrides iso-bnf-source-declaration-sources/precedences
                 iso-bnf-apply-rule-precedences iso-bnf-source-grammar-rules
                 iso-bnf-source-grammar-rules/overrides iso-bnf-source-literals
                 iso-bnf-source-syntax-kinds parse-iso-bnf-source/expected))
(export expand-iso-bnf-language expand-iso-bnf-vocabulary)

(def (expand-iso-bnf-language stx literal-tags compile-entry canonical-entry)
  (with-syntax ((compile-iso-bnf-language compile-entry)
                (compile-language canonical-entry))
    (syntax-case stx
      ()
    ((_ prefix (reference-tag source-version source-commit) section ...)
     (and (identifier? #'prefix)
          (language-expansion-tags? (list #'reference-tag)
                                    (list (cadr (syntax->list literal-tags)))))
     (with-syntax ((language-name (datum->syntax #'prefix (symbol->string (syntax->datum #'prefix)))))
       #'(compile-iso-bnf-language prefix (identity language-name #f #f)
           (reference source-version source-commit) section ...)))
    ((_ prefix
        (identity-tag language version contract)
        (reference-tag source-version source-commit)
        (digest-tag expected-digest)
        (source-tag path)
        (rule-overrides-tag override-row ...)
        (rule-precedences-tag precedence-row ...)
        (entrypoint-tag entry-name)
        (conflicts-tag conflict-policy)
        (case-insensitive-tag case-insensitive-value))
     (and (identifier? #'prefix)
          (language-expansion-tags?
           (list #'identity-tag #'reference-tag #'digest-tag #'source-tag #'rule-overrides-tag #'rule-precedences-tag #'entrypoint-tag #'conflicts-tag #'case-insensitive-tag)
           (syntax->list literal-tags))
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
  )

(def (expand-iso-bnf-vocabulary stx literal-tags compile-entry)
  (with-syntax ((compile-iso-bnf-language compile-entry))
    (syntax-case stx ()
    ((_ prefix
        (syntax-tag (reference-tag version commit) (digest-tag hash) (source-tag path)
                (rule-precedences-tag precedence ...)
                (entrypoint-tag root) (conflicts-tag policy) (case-insensitive-tag insensitive))
        (rules-tag overlay ...))
     (and (identifier? #'prefix)
          (language-expansion-tags?
           (list #'syntax-tag #'rules-tag #'reference-tag #'digest-tag #'source-tag #'rule-precedences-tag #'entrypoint-tag #'conflicts-tag #'case-insensitive-tag)
           (syntax->list literal-tags)))
     (with-syntax (((lowered ...) (lower-source-rule-overlays #'(overlay ...) stx #'prefix #'version #'commit))
                   ((lowered-precedence ...) (lower-source-rule-precedences #'(precedence ...) stx #'prefix #'version #'commit)))
       #'(compile-iso-bnf-language prefix
           (reference version commit) (digest hash) (source path)
           (rule-overrides lowered ...) (rule-precedences lowered-precedence ...)
           (entrypoint root) (conflicts policy) (case-insensitive insensitive))))
    (_ (raise-syntax-error #f "iso-bnf vocabulary requires checked source syntax and rule overlays" stx))))
  )
