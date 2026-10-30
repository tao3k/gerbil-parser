;;; Language-owned row, identity and kind Types are explicit constructor inputs.
(import (only-in :clan/poo/object .o .cc)
        (only-in :clan/poo/mop Fun Any Type element? Type.)
        (only-in :clan/poo/type Record List Exactly OrFalse TypeValuePair)
        (only-in :clan/poo/number UInt)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export +graph-query-context-kind+ GraphQueryAccessorOf GraphQueryPredicateOf
        GraphQueryViewContract GraphQueryContextContract
        graph-query-view? graph-query-context?)
(def +graph-query-context-kind+ 'gerbil-parser-graph-query-context)
(def (GraphQueryAccessorOf Row Value) (Fun Value <- Row))
;; Any is intentional ONLY for generalized Scheme truth values: filter accepts
;; #f or any true value, including a member tail. The input Row is always checked.
(def (GraphQueryPredicateOf Row) (Fun Any <- Row))
(def (GraphQueryViewContract Row Id Kind)
  (.cc YuanxuContract. identity: 'gerbil-parser/graph-query-view
    value-contract:
    (Record records: [(List Row)]
            id-of: [(GraphQueryAccessorOf Row Id)]
            parent-of: [(GraphQueryAccessorOf Row (OrFalse Id))]
            kind-of: [(GraphQueryAccessorOf Row Kind)])))
(def GraphQueryIndex (.cc Type. sexp: 'GraphQueryIndex '.element? hash-table?))
(def GraphQueryContextContract
  (.cc YuanxuContract. identity: 'gerbil-parser/graph-query-context proto: (.o)
    value-contract:
    (Record kind: [(Exactly +graph-query-context-kind+)]
            record-type: [Type] id-type: [Type] kind-type: [Type]
            view: [TypeValuePair] index: [GraphQueryIndex] count: [UInt])))
(def (graph-query-view? Row Id Kind value)
  (element? (GraphQueryViewContract Row Id Kind) value))
(def (graph-query-context? value) (element? GraphQueryContextContract value))
