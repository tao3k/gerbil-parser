#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/language-support
        :gerbil-parser/languages/tla-plus/parser)

(def metadata (language-parser-entry-ref tla-plus-layout-language 'metadata))
(def sany-source (language-metadata-ref metadata 'source-catalog))

(deflanguage-parser-tests javacc-source-tests "JavaCC native grammar source"
  (engine)
    (scenario "the complete pinned SANY production inventory is immutable"
      (equal (javacc-source-digest sany-source)
              (language-metadata-ref metadata 'sany-grammar-digest))
      (equal (length (javacc-source-productions sany-source))  99)
      (each (name) ('("CompilationUnit" "Module" "Body" "Expression" "LetIn"
         "Proof" "Step" "PrimitiveExp" "ExtendableExpr"
         "SBracketCases" "BangExt"))
         (matches (javacc-source-production sany-source name)
                 javacc-production?)))
    (scenario "duplicate production identities fail closed"
      (raises
       (parse-javacc-source
        "tiny" "v1" "commit"
       "SyntaxTreeNode\nRule() : {} {}\nSyntaxTreeNode\nRule() : {} {}\n")
       true))
    (scenario "Java action locals are not promoted to productions"
      (scope ((source
            (parse-javacc-source
             "tiny" "v1" "commit"
             (string-append
              "SyntaxTreeNode\nRule() : {\n"
              "  SyntaxTreeNode node;\n}{\n"
              "  if (node == null) { return node; }\n}\n"))))
        (equal (map javacc-production-name
                    (javacc-source-productions source))
                '("Rule"))))
    (scenario "declared source digest mismatch fails closed"
      (raises
       (parse-javacc-source/expected
        "tiny" "v1" "commit" "sha256:wrong"
        "SyntaxTreeNode\nRule() : {} {}\n")
       true)))


;; gxtest discovers only exported names ending in -test.
(def javacc-source-test javacc-source-tests)
(export javacc-source-test)
