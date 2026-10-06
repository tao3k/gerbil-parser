;;; Negative controls register faulty engines at the internal extension boundary.
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/result-profile ResultProfile. compile-result-profile)
        (only-in :gerbil-parser/src/language/source-strategy
                 SourceStrategy. declare-source-strategy-provider make-source-engine))
(export test-source-strategy)
(def (test-source-strategy factory scanner parser)
  (let (engine-provider
         (declare-source-strategy-provider 'test-engine (lambda (_) #t) (lambda (_) '(test-only))
          (lambda (_) (make-source-engine scanner factory parser #f
             (compile-result-profile (.o (:: self ResultProfile.) nodes: '((Test ())) tokens: '()))))))
    (.o (:: self SourceStrategy.) provider: engine-provider)))
