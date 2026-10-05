;;; Explicit POO pack entries expose grammar metadata and reusable test services.
(import :std/test
        (only-in :clan/poo/object .ref object?)
        (only-in :gerbil-parser/languages/arithmetic/v1/parser arithmetic-v1-language)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-language)
        (only-in :gerbil-parser/languages/gql/iso-39075-2024/parser gql-iso-39075-2024-language)
        (only-in :gerbil-parser/languages/cypher/opencypher-2024-1/parser opencypher-2024-1-language)
        (only-in :gerbil-parser/languages/bash/v5-3/parser bash-v5-3-language)
        (only-in :gerbil-parser/languages/hl7/v2-2.5.1/parser hl7v2-language)
        (only-in :gerbil-parser/languages/fhirpath/v2.0.0/parser fhirpath-v2-language)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-core-language tla-plus-layout-language)
        (only-in :gerbil-parser/languages/tla-plus/sany-candidate tla-plus-sany-candidate-language)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/language/entry make-language-scan-worker run-language-test +language-parser-entry-schema+)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-ir)
        (only-in :gerbil-parser/src/language/source source-language?))

;;; Count declarations in admitted IR, not filenames or copied result snapshots.
(def (external-site-count expression)
  (cond ((not (pair? expression)) 0)
        ((eq? (car expression) 'external) 1)
        (else (apply + (map external-site-count expression)))))
(def (loader-external-count loader)
  (let (descriptor (.ref loader 'descriptor))
    (if (source-language? descriptor) 'source-parser
      (external-site-count (cdr (assq 'lexical-rules (language-grammar-ir descriptor)))))))

(def language-pack-metadata-test
  (test-suite "public POO language pack metadata"
    (test-case "shipped entries expose their descriptor and POO metadata"
      (for-each
       (lambda (row)
         (let (loader (car row))
           (check (object? loader) => #t)
           (check (.ref loader 'schema) => +language-parser-entry-schema+)
           (check (not (not (memq (.ref loader 'descriptor) (.ref loader 'grammars)))) => #t)
           (check (.ref (.ref loader 'metadata) 'grammar-format) => (cdr row))))
       (list (cons arithmetic-v1-language 'concise-dsl)
             (cons hcl-v2-24-language 'concise-dsl)
             (cons hl7v2-language 'concise-dsl)
             (cons fhirpath-v2-language 'concise-dsl)
             (cons gql-iso-39075-2024-language 'antlr4)
             (cons opencypher-2024-1-language 'iso-bnf)
             (cons bash-v5-3-language 'source-parser)
             (cons tla-plus-core-language 'concise-dsl)
             (cons tla-plus-layout-language 'concise-dsl)
             (cons tla-plus-sany-candidate-language 'concise-dsl))))
    (test-case "the complete migration inventory follows all ten admitted descriptors"
      (check (map loader-external-count
                  (list arithmetic-v1-language hcl-v2-24-language
                        gql-iso-39075-2024-language opencypher-2024-1-language
                        hl7v2-language fhirpath-v2-language bash-v5-3-language
                        tla-plus-core-language tla-plus-layout-language tla-plus-sany-candidate-language))
             => '(0 0 0 0 8 5 source-parser 0 0 4)))
    (test-case "registered fixture services preserve native corpus conformance"
      (for-each
       (lambda (loader)
         (let (artifacts (run-language-test loader 'fixtures))
           (check (pair? artifacts) => #t)
           (check (every parse-artifact-valid? artifacts) => #t)))
       (list arithmetic-v1-language hcl-v2-24-language
             gql-iso-39075-2024-language opencypher-2024-1-language
             hl7v2-language fhirpath-v2-language bash-v5-3-language
             tla-plus-core-language tla-plus-layout-language tla-plus-sany-candidate-language)))
    (test-case "Bash workers retain independent deferred scanner obligations"
      (let* ((make-worker (lambda (source) (make-language-scan-worker bash-v5-3-language 'command source)))
             (first "cat <<EOF\nα\nEOF\n") (second "echo β\n")
             (left (make-worker first)) (right (make-worker second)))
        (check (apply string-append (map token-lexeme (source-scanner-tokens right 'command))) => second)
        (check (apply string-append (map token-lexeme (source-scanner-tokens left 'command))) => first)
        (check (apply string-append (map token-lexeme (source-scanner-tokens left 'command))) => first)))))
(export language-pack-metadata-test)
