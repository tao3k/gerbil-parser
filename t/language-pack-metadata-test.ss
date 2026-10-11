;;; Explicit POO pack entries expose grammar metadata and reusable test services.
(import (only-in :gerbil-parser/language-support/entry language-metadata-ref) (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-test-language)
        (only-in :gerbil-parser/languages/hcl/parser-test hcl-test-language)
        (only-in :gerbil-parser/languages/cypher/parser-test opencypher-test-language)
        (only-in :gerbil-parser/languages/tla-plus/parser-test tla-plus-core-test-language)
        (only-in :gerbil-parser/languages/bash/parser-test bash-test-language)
        (only-in :gerbil-parser/languages/hl7/parser-test hl7-test-language)
        (only-in :gerbil-parser/languages/fhirpath/parser-test fhirpath-test-language)
        (only-in :gerbil-parser/languages/tla-plus/parser-test tla-plus-layout-test-language)
        (only-in :gerbil-parser/languages/tla-plus/parser-test tla-plus-sany-candidate-test-language)
        :std/test
        (only-in :clan/poo/object .ref object?)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language)
        (only-in :gerbil-parser/languages/hcl/parser hcl-language)
        (only-in :gerbil-parser/languages/gql/parser gql-language)
        (only-in :gerbil-parser/languages/gql/parser-test gql-test-language)
        (only-in :gerbil-parser/languages/cypher/parser opencypher-language)
        (only-in :gerbil-parser/languages/bash/parser bash-language)
        (only-in :gerbil-parser/languages/hl7/parser hl7-language)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-language)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-core-language tla-plus-layout-language)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-sany-candidate-language)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/language-support/development make-language-scan-worker run-language-test +language-parser-entry-schema+)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-ir language-grammar-machine)
        (only-in :gerbil-parser/src/language/source source-language? source-language-digest)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-grammar-digest))

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
    (test-case "shipped entries expose their descriptor and value metadata"
      (for-each
       (lambda (row)
         (let (loader (car row))
           (check (object? loader) => #t)
           (for-each (lambda (key)
                       (check (language-metadata-ref (.ref loader 'metadata) key) => (.ref loader key)))
                     '(language version contract))
           (let (descriptor (.ref loader 'descriptor))
             (check (language-metadata-ref (.ref loader 'metadata) 'digest)
                    => (if (source-language? descriptor) (source-language-digest descriptor)
                         (parser-machine-grammar-digest (language-grammar-machine descriptor))))
             (check (language-metadata-ref (.ref loader 'metadata) 'digest-kind)
                    => (if (source-language? descriptor) 'source-identity 'parser-ir)))
           (check (.ref loader 'schema) => +language-parser-entry-schema+)
           (check (not (not (memq (.ref loader 'descriptor) (.ref loader 'grammars)))) => #t)
           (check (language-metadata-ref (.ref loader 'metadata) 'grammar-format) => (cdr row))))
       (list (cons arithmetic-language 'concise-dsl)
             (cons hcl-language 'concise-dsl)
             (cons hl7-language 'concise-dsl)
             (cons fhirpath-language 'concise-dsl)
             (cons gql-language 'antlr4)
             (cons opencypher-language 'iso-bnf)
             (cons bash-language 'source-parser)
             (cons tla-plus-core-language 'concise-dsl)
             (cons tla-plus-layout-language 'concise-dsl)
             (cons tla-plus-sany-candidate-language 'concise-dsl))))
    (test-case "the complete migration inventory follows all ten admitted descriptors"
      (check (map loader-external-count
                  (list arithmetic-language hcl-language
                        gql-language opencypher-language
                        hl7-language fhirpath-language bash-language
                        tla-plus-core-language tla-plus-layout-language tla-plus-sany-candidate-language))
             => '(0 0 0 0 0 0 source-parser 0 0 0)))
    (test-case "registered fixture services preserve native corpus conformance"
      (for-each
       (lambda (loader)
         (let (artifacts (run-language-test loader 'fixtures))
           (check (pair? artifacts) => #t)
           (check (every parse-artifact-valid? artifacts) => #t)))
       (list arithmetic-test-language hcl-test-language
             gql-test-language opencypher-test-language
             hl7-test-language fhirpath-test-language bash-test-language
             tla-plus-core-test-language tla-plus-layout-test-language tla-plus-sany-candidate-test-language)))
    (test-case "Bash workers retain independent deferred scanner obligations"
      (let* ((make-worker (lambda (source) (make-language-scan-worker bash-test-language 'command source)))
             (first "cat <<EOF\nα\nEOF\n") (second "echo β\n")
             (left (make-worker first)) (right (make-worker second)))
        (check (apply string-append (map token-lexeme (source-scanner-tokens right 'command))) => second)
        (check (apply string-append (map token-lexeme (source-scanner-tokens left 'command))) => first)
        (check (apply string-append (map token-lexeme (source-scanner-tokens left 'command))) => first)))))
(export language-pack-metadata-test)
