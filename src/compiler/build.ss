;;; Supported public entries. Core projects their native imports without runtime execution.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '("event-fold-scheme-modules")
               library-roots: '("src/compiler/event-fold-scheme-build"
                     "src/compiler/event-fold-scheme"
                     "src/compiler/rust-pure-aot"
                     "src/compiler/result-projection"
                     "src/compiler/contextual-parser-ir"
                     "src/compiler/decision-code"
                     "language-build-support"
                     "compiler-build-support"
                     "event-fold-build-support"
                     "event-fold-program-support"
                     "rust-runtime-support"
                     "line-structure-support"
                     "graph-projection-support")))
