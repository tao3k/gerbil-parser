;;; Supported public entries. Core projects their native imports without runtime execution.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '("source-execution")
               library-roots: '("src/testing/source-witness"
                     "src/testing/language-assertions"
                     "src/testing/language-product"
                     "src/testing/language-strategy"
                     "src/testing/language-source-assertions"
                     "language-test-support"
                     "language-support/fixture")))
