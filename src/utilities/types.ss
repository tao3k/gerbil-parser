;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Function Any Bool Type.)
        (only-in :clan/poo/type Record String List OrFalse)
        (only-in :clan/poo/number Integer)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export UtilitiesScopeOf UtilitiesScope UtilitiesScopeContract Character)

(define-type (Character @ Type.) .element?: char?)

;; Row and Key are supplied by each consumer; callbacks share those same types.
(def (UtilitiesScopeOf Row Key)
  (Record append-unique: [(Fun (List Row) <- (List Row) Row)]
          merge-keyed-row: [(Fun (List Row) <- (List Row) Row
                            (Fun Key <- Row) (Fun (List Row) <- Key))]
          ascii-whitespace?: [(Fun Bool <- Character)]
          string-prefix-at?: [(Fun Bool <- String String Integer)]
          string-index-from: [(Function [(OrFalse Integer)] [String String] optional: [Integer])]
          text-lines: [(Fun (List String) <- String)]))

;; The published general-purpose service accepts arbitrary row/key domains.
;; Consumers specialize through the same native Record, never another wrapper.
(define-type UtilitiesScope (UtilitiesScopeOf Any Any))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def UtilitiesScopeContract
  (.cc YuanxuContract. identity: 'utilities/scope
       proto: (.get UtilitiesScope proto) value-contract: UtilitiesScope))
