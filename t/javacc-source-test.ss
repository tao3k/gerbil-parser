#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/language-support
        :gerbil-parser/languages/tla-plus/v1/source)

(def javacc-source-tests
  (test-suite "JavaCC native grammar source"
    (test-case "the complete pinned SANY production inventory is immutable"
      (check (javacc-source-digest tla-plus-sany-source)
             => +tla-plus-sany-grammar-digest+)
      (check (length (javacc-source-productions tla-plus-sany-source)) => 99)
      (for-each
       (lambda (name)
         (check (javacc-source-production tla-plus-sany-source name)
                ? javacc-production?))
       '("CompilationUnit" "Module" "Body" "Expression" "LetIn"
         "Proof" "Step" "PrimitiveExp" "ExtendableExpr"
         "SBracketCases" "BangExt")))
    (test-case "duplicate production identities fail closed"
      (check-exception
       (parse-javacc-source
        "tiny" "v1" "commit"
       "SyntaxTreeNode\nRule() : {} {}\nSyntaxTreeNode\nRule() : {} {}\n")
       true))
    (test-case "Java action locals are not promoted to productions"
      (let (source
            (parse-javacc-source
             "tiny" "v1" "commit"
             (string-append
              "SyntaxTreeNode\nRule() : {\n"
              "  SyntaxTreeNode node;\n}{\n"
              "  if (node == null) { return node; }\n}\n")))
        (check (map javacc-production-name
                    (javacc-source-productions source))
               => '("Rule"))))
    (test-case "declared source digest mismatch fails closed"
      (check-exception
       (parse-javacc-source/expected
        "tiny" "v1" "commit" "sha256:wrong"
        "SyntaxTreeNode\nRule() : {} {}\n")
       true))))

(run-tests! javacc-source-tests)
