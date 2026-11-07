;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type Fun Any Bool Type Object)
        (only-in :clan/poo/type Record))
(import (only-in "./graph-query-types.ss" GraphQueryContextContract))
(export GraphScope GraphScopeContract)

;; The last view? argument is an arbitrary candidate; Row/Id/Kind remain explicit Types.
(define-type GraphScope
  (Record make-graph-query-context: [(Fun GraphQueryContextContract <- Type Type Type Object)]
          graph-query-view?: [(Fun Bool <- Type Type Type Any)]))

(def GraphScopeContract
  (.cc GraphScope identity: 'graph/scope))
