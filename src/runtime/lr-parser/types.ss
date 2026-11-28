(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Fun Bool Any Record))
(export LrParserScopeContract)

(define-type LrParserScope
  (Record lr-runtime?: [(Fun Bool <- Any)]))

(def LrParserScopeContract
  (.cc LrParserScope identity: 'lr-parser/scope))
