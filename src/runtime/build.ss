;;; Supported public entries. Core projects their native imports without runtime execution.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '("fold-tape" "fold-cost")
               library-roots: '("parser-actor-support"
                     "rust-runtime-event-support"
                     "src/runtime/incremental"
                     "src/runtime/recovery"
                     "src/runtime/source-engines")))
