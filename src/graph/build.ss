;;; Graph declarations and queries are published by their domain owner.
(import (only-in :clan/poo/object .o))
(export Build)
(def Build (.o kind: 'module/build roots: '()
               library-roots: '("graph-query-support" "graph-syntax-support")))
