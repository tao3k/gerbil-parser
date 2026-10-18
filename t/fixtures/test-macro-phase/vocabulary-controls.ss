;;; Prepare before author-controls: compiled vocabulary consumers remain dormant.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/languages/gql/parser parse-gql +gql-representative-query+)
        (only-in :gerbil-parser/languages/cypher/parser parse-opencypher +opencypher-representative-query+)
        (only-in :gerbil-parser/languages/bash/parser parse-bash)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-roundtrip))

(def +author-services-loaded+
  (map (lambda (name) (and (assoc name (list-modules)) #t))
       '("gerbil-parser/src/compiler/language-declaration"
         "gerbil-parser/src/compiler/antlr4-declaration"
         "gerbil-parser/src/compiler/iso-bnf-declaration"
         "gerbil-parser/src/compiler/source-language")))

(deflanguage-parser-tests vocabulary-phase-test "compiled vocabulary phase boundary"
  (engine (exclusive #t))
  (property "compiled vocabularies do not initialize author services"
    (bindings)
    (equal +author-services-loaded+ '(#f #f #f #f)))
  (matrix "compiled vocabulary products retain complete requests"
    (setup)
    ((request (list (cons parse-gql +gql-representative-query+)
                    (cons parse-opencypher +opencypher-representative-query+)
                    (cons parse-bash "echo ok\n"))))
    (bindings (artifact ((car request) (cdr request))))
    (equal (parse-artifact-success? artifact) #t)
    (equal (parse-artifact-roundtrip artifact) (cdr request))))
