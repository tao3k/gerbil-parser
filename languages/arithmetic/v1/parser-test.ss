;;; -*- Gerbil -*-
;;; Arithmetic language-pack entry and native fixture contract.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/language/entry
                 +language-parser-entry-schema+ language-parser-entry-ref)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-ref parse-artifact-roundtrip
                 parse-artifact-success?)
        (only-in :gerbil-parser/language-support
                 syntax-fixture-source syntax-fixture-source-digest)
        (only-in :gerbil-parser/src/testing/parser-ast check-parser-ast)
        (only-in ./fixtures arithmetic-v1-basic-fixture)
        (only-in ./parser arithmetic-v1-language parse-arithmetic-v1))
(export arithmetic-v1-parser-test)

(def arithmetic-v1-parser-test
  (test-suite "arithmetic v1 language pack"
    (test-case "the declarative entry parses its colocated native fixture"
      (let* ((fixture arithmetic-v1-basic-fixture)
             (source (syntax-fixture-source fixture))
             (artifact (parse-arithmetic-v1 source)))
        (check (language-parser-entry-ref arithmetic-v1-language 'schema)
               => +language-parser-entry-schema+)
        (check (language-parser-entry-ref arithmetic-v1-language 'language)
               => "arithmetic")
        (check (language-parser-entry-ref arithmetic-v1-language 'version)
               => "v1")
        (check (language-parser-entry-ref arithmetic-v1-language 'contract)
               => "arithmetic-expression.v1")
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-ref artifact 'sourceDigest)
               => (syntax-fixture-source-digest fixture))
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "canonical AST shape is an exact parser contract"
      (check-parser-ast (parse-arithmetic-v1 "1") "1"
        (node SourceFile 0 1
          (field expression 0 1
            (node NumberExpression 0 1
              (field value 0 1
                (token number "1" 0 1)))))))))
