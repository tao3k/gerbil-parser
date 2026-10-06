;;; -*- Gerbil -*-
;;; Expansion-time ANTLR4 language-pack compiler.

(import (for-syntax :std/misc/ports
                    (only-in :gerbil-parser/src/compiler/language-artifact
                             make-language-declaration expand-language-declaration-syntax)
                    (only-in :gerbil-parser/src/compiler/parser-ir compile-parser)
                    (only-in :gerbil-parser/src/compiler/bound-ir bind-grammar-ir)
                    (only-in ./antlr4-source
                             antlr4-source->datum
                             antlr4-source-declaration-sources
                             antlr4-source-parser-grammar-rules
                             antlr4-source-parser-literals
                             antlr4-source-parser-syntax-kinds
                             parse-antlr4-source/expected))
        (only-in ./antlr4-source antlr4-source-from-datum)
        (only-in :gerbil-parser/src/language/assembly assemble-language-parser)
        (only-in :gerbil-parser/src/language/descriptor make-language-grammar))
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
;;         (entrypoint program) (conflicts reject) (case-insensitive #f)
;;         (lexical-profile
;;           (token-bindings ("NAME" token identifier))
;;           (syntax-kinds (Name token (text)) (Punctuation token (text)))
;;           (terminals (identifier Name) (punctuation Punctuation))
;;           (lexical-rules (identifier (identifier))
;;                          (punctuation (source-literals)))
;;           (extras)))
;;       ;; => source catalog, grammar, Parser IR, machine, and descriptor bindings
;;       ```
;;       Result: prefix-antlr4-source and parser products share one accepted source.
;;       Runtime reconstructs the typed catalog from data; it never reparses source.
;;     %
(defsyntax (deflanguage-antlr4-grammar stx)
  (syntax-case stx
      (identity reference digest source entrypoint conflicts case-insensitive lexical-profile
                token-bindings syntax-kinds terminals lexical-rules extras)
    ((_ prefix
        (identity language version contract)
        (reference source-version source-commit)
        (digest expected-digest)
        (source path)
        (entrypoint entry-name)
        (conflicts conflict-policy)
        (case-insensitive case-insensitive-value)
        (lexical-profile (token-bindings binding ...)
                        (syntax-kinds kind ...)
                        (terminals terminal ...)
                        (lexical-rules lexical ...)
                        (extras extra ...)))
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
            (bindings (syntax->datum #'(binding ...)))
            (syntax-rows
             (append (antlr4-source-parser-syntax-kinds catalog)
                     (syntax->datum #'(kind ...))))
            (rule-rows (antlr4-source-parser-grammar-rules catalog bindings))
            (literal-values (antlr4-source-parser-literals catalog bindings))
            (source-map
             (antlr4-source-declaration-sources catalog (stx-e #'path))))
       ;; Submit the accepted source as a structured declaration. The shared
       ;; compiler owns canonical admission, binding, publication and emission.
       (let* ((sections
               `((syntax-kinds ,@syntax-rows)
                 (terminals ,@(syntax->datum #'(terminal ...)))
                 (lexical-rules
                  ,@(map (lambda (row)
                           (if (and (list? row) (= (length row) 2)
                                    (equal? (cadr row) '(source-literals)))
                             (list (car row) (cons 'literals literal-values))
                             row))
                         (syntax->datum #'(lexical ...))))
                 (rules ,@rule-rows)
                 (extras ,@(syntax->datum #'(extra ...))) (keywords)
                 (parser-entrypoints (,(stx-e #'entry-name) parse pure))
                 (recoveries
                  (,(stx-e #'entry-name) "GERBIL-PARSER-ANTLR4-SOURCE" preserve-source))
                 (flow (source lexical) (lexical cst))))
              (declaration
               (make-language-declaration
                stx #'prefix (syntax->list #'(language version contract))
                (map (lambda (row)
                       (cons (car row) (datum->syntax #'prefix (cdr row)))) sections)
                (syntax->datum #'conflict-policy)
                (syntax->datum #'case-insensitive-value)
                `(deflanguage-antlr4-grammar ,(stx-e #'source-version)
                                            ,(stx-e #'source-commit))
                source-map)))
         (with-syntax ((catalog-binding
                        (datum->syntax #'prefix
                         (string->symbol
                          (string-append (symbol->string (stx-e #'prefix))
                                         "-antlr4-source"))))
                       (catalog-data (antlr4-source->datum catalog))
                       (profile-binding (datum->syntax #'prefix
                         (string->symbol (string-append (symbol->string (stx-e #'prefix))
                                                      "-antlr4-token-bindings"))))
                       (profile-data bindings)
                       (compiled-declaration
                        (expand-language-declaration-syntax
                         declaration compile-parser bind-grammar-ir
                         #'assemble-language-parser #'make-language-grammar #'begin #'def)))
           #'(begin
               (def profile-binding 'profile-data)
               (def catalog-binding (antlr4-source-from-datum 'catalog-data))
               compiled-declaration)))))
    (_ (raise-syntax-error #f "invalid ANTLR4 language declaration" stx))))
