(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Fun Bool Any Record))
(export IncrementalScopeContract)

(define-type IncrementalScope
  (Record incremental-session?: [(Fun Bool <- Any)]))

(def IncrementalScopeContract
  (.cc IncrementalScope identity: 'incremental/scope))
