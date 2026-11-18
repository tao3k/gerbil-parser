(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type Function Record Symbol Type. define-type)
        (only-in :clan/poo/io module-action)
        (only-in ../../src/grammar/declaration-types ParserList))
(export EventFoldScopeContract)

(def compiled-value?
  (module-action "gerbil-parser/src/compiler/event-fold-program"
                 'gerbil-parser/src/compiler/event-fold-program#event-fold-program?))

(define-type (EventFoldProgram @ Type.)
  .element?: (lambda (value) (compiled-value? (list value))))

(define-type EventFoldScope
  (Record prepare:
    [(Function [EventFoldProgram] [Symbol ParserList ParserList ParserList]
       optional: [ParserList])]))

(def EventFoldScopeContract (.cc EventFoldScope identity: 'event-fold/scope))
