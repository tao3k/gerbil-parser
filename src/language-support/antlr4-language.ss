;;; -*- Gerbil -*-
;;; Expansion-time ANTLR4 language-pack compiler.

(import (for-syntax :std/misc/ports
                    (only-in ./antlr4-source
                             antlr4-source-declaration-sources
                             antlr4-source-parser-grammar-rules
                             antlr4-source-parser-literals
                             antlr4-source-parser-syntax-kinds
                             parse-antlr4-source/expected))
        :gerbil-parser/src/language/grammar)
(export deflanguage-antlr4-grammar)

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
;;       Result: runtime receives immutable v1 data and never provider source.
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
