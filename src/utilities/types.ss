;;; Fixed services are concrete; polymorphic services require explicit Types.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Function Bool Type Type.)
        (only-in :clan/poo/type Record String List OrFalse TypeValuePair)
        (only-in :clan/poo/number Integer)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(export ListOperationsScopeOf UtilitiesScope UtilitiesScopeContract Character)

(define-type (Character @ Type.) .element?: char?)

;; A caller-owned struct Type works here as well as String or another Record.
(def (ListOperationsScopeOf Row Key)
  (Record append-unique: [(Fun (List Row) <- (List Row) Row)]
          merge-keyed-row: [(Fun (List Row) <- (List Row) Row
                            (Fun Key <- Row) (Fun (List Row) <- Key))]))

;; The native dependent pair retains the selected Type and its checked value.
;; There is no default row/key domain and no untyped list-service publication.
(define-type UtilitiesScope
  (Record list-operations-for: [(Fun TypeValuePair <- Type Type)]
          ascii-whitespace?: [(Fun Bool <- Character)]
          string-prefix-at?: [(Fun Bool <- String String Integer)]
          string-index-from: [(Function [(OrFalse Integer)] [String String] optional: [Integer])]
          text-lines: [(Fun (List String) <- String)]))

(def UtilitiesScopeContract
  (.cc YuanxuContract. identity: 'utilities/scope
       proto: (.get UtilitiesScope proto) value-contract: UtilitiesScope))
