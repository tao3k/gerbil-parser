;;; Supported public entries. Core projects their native imports without runtime execution.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '()
               library-roots: '("rust-runtime-grammar-support")))
