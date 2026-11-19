;;; Public Fold program data and request-owned graph effects.
(import ../../src/compiler/event-fold-program
        (only-in ../../src/runtime/fold-graph
                 make-fold-graph fold-graph-start! fold-graph-token!
                 fold-graph-finish! fold-graph-publish!))
(export (import: ../../src/compiler/event-fold-program)
        make-fold-graph fold-graph-start! fold-graph-token!
        fold-graph-finish! fold-graph-publish!)
