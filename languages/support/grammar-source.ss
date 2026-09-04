;;; -*- Gerbil -*-
;;; Declarative expansion-time inclusion of external grammar sources.

(import (for-syntax :std/misc/ports
                    ./antlr4-source)
        ./iso-bnf
        ./antlr4-source
        :gerbil-parser/src/language/grammar)
(export defsyntax-iso-bnf-source
        defsyntax-antlr4-source
        deflanguage-antlr4-grammar)

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

;; ANTLR source text is parsed and digest-checked by the expander. The runtime
;; value reconstructs only the immutable typed catalog; it never reads or
;; reparses the native grammar file.
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

;; A language pack declares identity and a pinned native grammar once. The
;; expander materializes the typed catalog, canonical Grammar IR, LALR table,
;; and generated machine; runtime code receives immutable data only.
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
            (literal-values (antlr4-source-parser-literals catalog)))
       (with-syntax ((((syntax-row ...)) (list syntax-rows))
                     (((rule-row ...)) (list rule-rows))
                     (((literal-value ...)) (list literal-values)))
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
              (string (quoted-string "\"" "'"))
              (number (number))
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
             (flow (source lexical) (lexical cst))))))
    (_ (raise-syntax-error #f "invalid ANTLR4 language declaration" stx))))
