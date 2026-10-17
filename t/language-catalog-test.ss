(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Published ABI order is validated over the concise DSL's inferred syntax.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/src/compiler/language-expander compile-language)
        (only-in :gerbil-parser/src/compiler/normalize grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-events parse-artifact-success? parse-artifact-roundtrip))

(begin
 (deflanguage projected
  (syntax
   (lexical
    (root source-file)
    (lex (word WordToken (identifier))
       (whitespace Whitespace (whitespace+))
       (unknown Unknown (fallback)))
    (extras whitespace)
    (catalog
   (syntax-kinds (Root node (reserved item)) (WordToken token (text))
                 (EmptyLine node ()) (Name node (value))
                 (Unknown token (text)) (Whitespace token (text)) (UnusedToken token (text)))
   (terminals (unknown Unknown) (word WordToken) (whitespace Whitespace))
   (reserved-token-kinds UnusedToken))))
  (rules (source-file (node Root (field item name)))
         (name (node Name (field value word)))))
 (bind-fixture-grammar-release projected "catalog-witness" "v1" "catalog-witness.v1") )

(compile-language published
  (identity "catalog-witness" "v1" "catalog-witness.v1")
  (syntax-kinds (Root node (reserved item)) (WordToken token (text))
                (EmptyLine node ()) (Name node (value))
                (Unknown token (text)) (Whitespace token (text)) (UnusedToken token (text)))
  (terminals (unknown Unknown) (word WordToken) (whitespace Whitespace))
  (lexical-rules (word (identifier)) (whitespace (whitespace+)) (unknown (fallback)))
  (rules (source-file (alias Root (field item (reference name))))
         (name (alias Name (field value (token word)))))
  (extras whitespace) (keywords) (parser-entrypoints (source-file parse pure))
  (recoveries) (conflicts reject) (case-insensitive #f)
  (flow (source lexical) (lexical parser) (parser cst)))

(defsyntax (catalog-rejection-messages stx)
  (def (message option)
    (with-syntax ((catalog-option (datum->syntax #'deflanguage option)))
      (with-catch (lambda (condition) (error-message condition))
        (lambda ()
          (core-expand
           #'(deflanguage invalid-catalog
  (syntax
   (lexical
    (root source-file)
    (lex (word WordToken (identifier)))
    catalog-option))
  (rules (source-file (node Root (field value word))))))
          "UNEXPECTED-ADMISSION"))))
  (datum->syntax
   #'catalog-rejection-messages
   (list 'quote
         (map message
          '((catalog (syntax-kinds) (terminals (word WordToken)))
            (catalog (syntax-kinds (WordToken token (text)) (Root node (value)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node (value)) (Root node (value)) (WordToken token (text)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node ()) (WordToken token (text)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node (value)) (WordToken node (text)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node (value value)) (WordToken token (text)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)) (Invented token (text)))
                     (terminals (word WordToken)))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)))
                     (terminals (word Root)))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)))
                     (terminals (word WordToken) (word WordToken)))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)))
                     (terminals (word WordToken)) (reserved-token-kinds WordToken))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)))
                     (terminals (word WordToken)) (reserved-token-kinds Missing))
            (catalog (syntax-kinds (Root node (value)) (WordToken token (text)))))))))

(deflanguage-parser-tests language-catalog-test "validated published language catalog"
  (engine)
    (property "retains reserved nodes, fields and interleaved public kind IDs"
      (bindings)
      (equal (grammar-ir-ref projected-grammar 'syntax-kinds)
              (grammar-ir-ref published-grammar 'syntax-kinds))
      (equal (parser-ir-ref projected-parser-ir 'root-kind)  'Root)
      (equal (grammar-ir-ref projected-grammar 'terminals)
              (grammar-ir-ref published-grammar 'terminals)))
    (scenario "preserves canonical rules, lexical priority and LR tables"
      (each (key) ('(lexical-rules rules extras keywords parser-entrypoints recoveries conflicts case-insensitive flow))
         (equal (grammar-ir-ref projected-grammar key)  (grammar-ir-ref published-grammar key)))
      (equal (parser-ir-ref projected-parser-ir 'lr-spec)  (parser-ir-ref published-parser-ir 'lr-spec)))
    (scenario "preserves accepted and rejected lossless artifacts"
      (each (source) ('("alpha" " α " "?" "alpha beta" ""))
         (scope ((actual (parse-source projected-parser source))
               (expected (parse-source published-parser source)))
           (equal (parse-artifact-success? actual)  (parse-artifact-success? expected))
           (equal (parse-artifact-roundtrip actual)  source)
           (equal (parse-artifact-events actual)  (parse-artifact-events expected)))))
    (scenario "rejects incomplete, remapped and malformed catalogs during expansion"
      (scope ((messages (catalog-rejection-messages)))
        (equal (length messages)  12)
        (each (message) (messages)
           (equal (not (not (string-contains message "catalog")))  #t)
           (equal (string-contains message "UNEXPECTED-ADMISSION")  #f)))))
