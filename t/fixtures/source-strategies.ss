;;; Negative controls register faulty engines at the internal extension boundary.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/result-profile ResultProfile. compile-result-profile)
        (only-in :gerbil-parser/src/language/source-strategy
                 SourceStrategy. declare-source-strategy-provider make-source-engine)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/src/compiler/contextual-program compile-line-contextual-program contextual-program-ir contextual-program-results))
(export test-source-strategy)
(def (test-source-strategy factory scanner parser)
  (let (engine-provider
         (declare-source-strategy-provider 'test-engine (lambda (_) #t)
          (lambda (_)
            (let (program (compile-line-contextual-program
                           (.o (:: self LineSourceStrategy.) root-kind: 'Test token-kind: 'TestToken)))
              (values (contextual-program-ir program)
                (make-source-engine scanner factory parser #f
                  (contextual-program-results program) 'Test program))))))
    (.o (:: self SourceStrategy.) provider: engine-provider)))
