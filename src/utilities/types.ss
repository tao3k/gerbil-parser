;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Function Any Bool)
        (only-in :clan/poo/type Record String List OrFalse)
        (only-in :clan/poo/number Integer)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export UtilitiesScope UtilitiesScopeContract)

(define-type UtilitiesScope
  (Record append-unique: [(Fun (List Any) <- (List Any) Any)]
          merge-keyed-row: [(Fun Any <- (List Any) Any Any Any)]
          ascii-whitespace?: [(Fun Bool <- Any)]
          string-prefix-at?: [(Fun Bool <- String String Integer)]
          string-index-from: [(Function [(OrFalse Integer)] [String String] optional: [Integer])]
          text-lines: [(Fun (List Any) <- String)]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def UtilitiesScopeContract
  (.cc YuanxuContract. identity: 'utilities/scope
       proto: (.get UtilitiesScope proto) value-contract: UtilitiesScope))
