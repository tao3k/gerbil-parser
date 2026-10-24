;;; -*- Gerbil -*-
;;; Callable extension boundaries shared by scanner and LR preparation.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/mop Type. Function Fun Any Bool)
        (only-in :clan/poo/type String Symbol OrFalse)
        (only-in :clan/poo/number UInt)
        (only-in ./token token?))
(export SourceScannerStep ActionSelector ActionSelectorFactory)

;; The scanner returns kind/end/context as three values. Relative progress and
;; source bounds remain algorithmic invariants, rather than pretending to be Types.
(def SourceScannerStep
  (Function (list (OrFalse Symbol) UInt Any) (list String UInt Any Any)))
(def ParserToken (.cc Type. sexp: 'ParserToken '.element? token?))
(def ActionRows (.cc Type. sexp: 'LRActionRows '.element? vector?))
(def ActionSelector (Fun Any <- UInt (OrFalse ParserToken)))
;; A checked factory produces a checked selector through native recursive
;; Function validation; there is no second handwritten callback protocol.
(def ActionSelectorFactory (Fun ActionSelector <- ActionRows ActionRows Bool Bool))
