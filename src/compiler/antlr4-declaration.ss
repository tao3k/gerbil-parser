;;; Author-time ANTLR4 service. Runtime identifiers are caller-owned operands.
(import (only-in :gerbil/core/expander
                 syntax-case syntax with-syntax datum->syntax syntax->datum syntax->list
                 stx-string? stx-e stx-source stx-map identifier? raise-syntax-error)
        (only-in ../language/expansion-service language-expansion-tags?)
        (only-in :std/misc/ports read-all-as-string)
        (only-in ./language-artifact make-language-declaration expand-language-declaration-syntax
                 materialize-compiled-language-artifact encode-compiled-language-artifact)
        (only-in ./parser-ir compile-parser)
        (only-in ./bound-ir bind-grammar-ir)
        (only-in ../../language-support/antlr4-source antlr4-source->datum
                 antlr4-source-declaration-sources antlr4-source-parser-grammar-rules
                 antlr4-source-parser-literals antlr4-source-parser-syntax-kinds
                 parse-antlr4-source/expected))
(export expand-antlr4-language expand-antlr4-vocabulary)

(def (expand-antlr4-language stx literal-tags compile-entry catalog-entry load-entry
                                assembly-entry descriptor-entry begin-entry define-entry)
  (with-syntax ((compile-antlr4-language compile-entry)
                (antlr4-source-from-datum catalog-entry)
                (load-compiled-language-artifact/embedded load-entry)
                (assemble-language-parser assembly-entry)
                (make-language-grammar descriptor-entry)
                (begin begin-entry) (def define-entry))
    (syntax-case stx
      ()
    ((_ prefix (reference-tag source-version source-commit) section ...)
     (and (identifier? #'prefix)
          (language-expansion-tags? (list #'reference-tag)
                                    (list (cadr (syntax->list literal-tags)))))
     (with-syntax ((language-name (datum->syntax #'prefix (symbol->string (syntax->datum #'prefix)))))
       #'(compile-antlr4-language prefix (identity language-name #f #f)
           (reference source-version source-commit) section ...)))
    ((_ prefix
        (identity-tag language version contract)
        (reference-tag source-version source-commit)
        (digest-tag expected-digest)
        (source-tag path)
        (entrypoint-tag entry-name)
        (conflicts-tag conflict-policy)
        (case-insensitive-tag case-insensitive-value)
        (lexical-profile-tag (token-bindings-tag binding ...)
                        (syntax-kinds-tag kind ...)
                        (terminals-tag terminal ...)
                        (lexical-rules-tag lexical ...)
                        (extras-tag extra ...)))
     (and (identifier? #'prefix)
          (language-expansion-tags?
           (list #'identity-tag #'reference-tag #'digest-tag #'source-tag #'entrypoint-tag #'conflicts-tag #'case-insensitive-tag #'lexical-profile-tag #'token-bindings-tag #'syntax-kinds-tag #'terminals-tag #'lexical-rules-tag #'extras-tag)
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
             (parse-antlr4-source/expected
              (stx-e #'language) (stx-e #'source-version)
              (stx-e #'source-commit) (stx-e #'expected-digest) content))
            (bindings (syntax->datum #'(binding ...)))
            (catalog-image
             (materialize-compiled-language-artifact
              `((schema . "gerbil-parser.source-catalog-image.v1")
                (value . ,(antlr4-source->datum catalog)))))
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
                `(compile-antlr4-language ,(stx-e #'source-version)
                                            ,(stx-e #'source-commit))
                source-map)))
         (with-syntax ((catalog-binding
                        (datum->syntax #'prefix
                         (string->symbol
                          (string-append (symbol->string (stx-e #'prefix))
                                         "-antlr4-source"))))
                       (catalog-locator (datum->syntax #'prefix catalog-image))
                       (catalog-payload (datum->syntax #'prefix (encode-compiled-language-artifact catalog-image)))
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
               (def catalog-binding
                 (antlr4-source-from-datum
                  (cdr (assq 'value
                    (load-compiled-language-artifact/embedded
                     "gerbil-parser.source-catalog-image.v1" 'catalog-locator 'catalog-payload)))))
               compiled-declaration)))))
    (_ (raise-syntax-error #f "invalid ANTLR4 language declaration" stx))))
  )

(def (expand-antlr4-vocabulary stx literal-tags compile-entry expand-text-lexical-rows)
  (with-syntax ((compile-antlr4-language compile-entry))
    (syntax-case stx ()
    ((_ prefix
        (syntax-tag (reference-tag version commit) (digest-tag hash) (source-tag path)
                (entrypoint-tag root) (conflicts-tag policy) (case-insensitive-tag insensitive)
                (token-bindings-tag binding ...) (lex-tag lexical-row ...) (extras-tag extra ...))
        (rules-tag))
     (and (identifier? #'prefix)
          (language-expansion-tags?
           (list #'syntax-tag #'rules-tag #'reference-tag #'digest-tag #'source-tag #'entrypoint-tag #'conflicts-tag #'case-insensitive-tag #'token-bindings-tag #'lex-tag #'extras-tag)
           (syntax->list literal-tags)))
     (let* ((rows (expand-text-lexical-rows #'(lexical-row ...) stx))
            (datum (syntax->datum rows)))
       (unless (and (list? datum)
                    (andmap (lambda (row) (and (list? row) (= (length row) 3)
                                               (symbol? (car row)) (symbol? (cadr row)))) datum))
         (raise-syntax-error #f "antlr4 lexical declarations require token, kind and recognition" stx))
       (with-syntax (((kind ...) (datum->syntax #'prefix
                                  (map (lambda (row) (list (cadr row) 'token '(text))) datum)))
                     ((terminal ...) (datum->syntax #'prefix
                                      (map (lambda (row) (list (car row) (cadr row))) datum)))
                     ((lexical ...) (datum->syntax #'prefix
                                     (map (lambda (row) (list (car row) (caddr row))) datum))))
         #'(compile-antlr4-language prefix
             (reference version commit) (digest hash) (source path)
             (entrypoint root) (conflicts policy) (case-insensitive insensitive)
             (lexical-profile (token-bindings binding ...) (syntax-kinds kind ...)
                             (terminals terminal ...) (lexical-rules lexical ...) (extras extra ...))))))
    (_ (raise-syntax-error #f
         "antlr4 vocabulary requires checked source syntax, one lexical declaration and an empty local rules block" stx))))
  )
