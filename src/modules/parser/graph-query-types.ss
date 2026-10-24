;;; -*- Gerbil -*-
;;; Native Records install checked language-owned graph callbacks at admission.

(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Fun Any element? validate Type.)
        (only-in :clan/poo/type Record List Exactly)
        (only-in :clan/poo/number UInt)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export +graph-query-context-kind+
        GraphQueryRecord GraphQueryAccessor GraphQueryPredicate
        GraphQueryViewContract GraphQueryContextContract
        graph-query-view? graph-query-context?)

(def +graph-query-context-kind+ 'gerbil-parser-graph-query-context)
(def GraphQueryRecord (Record))
;; IDs, parents and kinds belong to the language. Predicates follow Scheme's
;; ordinary truth-value convention; the callback still takes exactly one record.
(def GraphQueryAccessor (Fun Any <- GraphQueryRecord))
(def GraphQueryPredicate (Fun Any <- GraphQueryRecord))
(def GraphQueryViewContract
  (.cc YuanxuContract. identity: 'gerbil-parser/graph-query-view
    value-contract:
    (Record records: (list (List GraphQueryRecord))
            id-of: (list GraphQueryAccessor)
            parent-of: (list GraphQueryAccessor)
            kind-of: (list GraphQueryAccessor))))
(def GraphQueryIndex (.cc Type. sexp: 'GraphQueryIndex '.element? hash-table?))
(def GraphQueryContextContract
  (.cc YuanxuContract. identity: 'gerbil-parser/graph-query-context proto: (.o)
    value-contract:
    (Record kind: (list (Exactly +graph-query-context-kind+))
            view: (list GraphQueryViewContract)
            index: (list GraphQueryIndex) count: (list UInt))))

(def (graph-query-view? value) (element? GraphQueryViewContract value))
(def (graph-query-context? value) (element? GraphQueryContextContract value))
