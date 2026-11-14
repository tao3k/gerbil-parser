(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop define-type Fun)
        (only-in :clan/poo/type Record OrFalse String)
        (only-in :clan/poo/number UInt))
(export ScannerScopeContract)

(define-type ScannerScope
  (Record scan-line: [(Fun (OrFalse UInt) <- String UInt)]))

(def ScannerScopeContract
  (.cc ScannerScope identity: 'scan/scope))
