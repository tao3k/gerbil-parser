;;; Supported public entries. Core projects their native imports without runtime execution.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '()
               library-roots: '("src/api" "src/declarations"
                     "language-support"
                     "language-source-support"
                     "language-support/iso-bnf-language"
                     "language-support/antlr4-language"
                     "language-support/javacc-source"
                     "languages/arithmetic/parser"
                     "languages/bash/parser"
                     "languages/cypher/parser"
                     "languages/fhirpath/parser"
                     "languages/gql/parser"
                     "languages/hcl/parser"
                     "languages/hl7/parser"
                     "languages/tla-plus/parser")))
